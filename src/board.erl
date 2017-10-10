-module(board).
-compile(export_all).
-record(state, {frames, stage, cells, size, x, y}).

% The GOL has three stages:
%   - check_ns, in which each cell checks its neighbors and decides whether it
%     will live or die, 
%   - alter_st, in which each cell flips over from its current state to its next
%     state, and
%   - report, in which each cell reports its {index, life} up to the board for
%     printing,
% We cycle between these three stages until all frames have been rendered.

% Starts a GOL with Frames (an integer) frames left to live.
% Its initial state is a list of 1s or 0s, and it has dimensions X by Y.
init(Initial_State, Frames, {X, Y}) ->

  io:format("                                                    _        ~n"),
  io:format("                                                   | |       ~n"),
  io:format("  ___ ___  _ ____      ____ _ _   _        ___ _ __| |       ~n"),
  io:format(" / __/ _ \\| '_ \\ \\ /\\ / / _` | | | |      / _ \\ '__| |  ~n"),
  io:format("| (_| (_) | | | \\ V  V / (_| | |_| |  _  |  __/ |  | |      ~n"),
  io:format(" \\___\\___/|_| |_|\\_/\\_/ \\__,_|\\__, | (_)  \\___|_|  |_|~n"),
  io:format("                               __/ |                         ~n"),
  io:format("                              |___/                        ~n~n"),

  io:format("Drawing ~w frames on an ~w x ~w grid...~n",[Frames, X, Y]),
  out:cleanOutputFolder(),
  Size = X * Y,
  Is   = lists:seq(1,Size),
  io:format("Initializing ~w cells...~n",[Size]),
  Cs   = [ cell:init(I,L) || {I,L} <- lists:zip(Is,Initial_State) ],
  S    = #state{frames=Frames, stage=report, size=Size, cells=Cs, x=X, y=Y},
  % Once we have created the cells, we need to broadcast out a list of their
  % neighbor's PIDs to them.
  io:format("Linking      ~w cells...~n",[Size]),
  [ C ! {moore, neighbors:getNPids(I,X,Y,Cs)} || {I,C} <- lists:zip(Is,Cs) ],
  io:format("Linked       ~w cells...~n",[Size]),
  action(S).

% When a board has no frames left to run, it dies.
action(#state{frames=0}) -> 
    io:format("Writing out to /tmp/conway/gif.gif.~n")
  , os:cmd("convert -delay 20 -loop 0 $(ls /tmp/conway/frame_* | tac ) /tmp/conway/gif.gif")
  , io:format("Wrote   out to /tmp/conway/gif.gif.~n")
  ;

% Sometimes a board has an action it needs to take.
action(S) ->
  case S#state.stage of
    % If we need to collect the states of all the cells, we create a Collector
    % which knows to report back to the Board when it's done. We tell the
    % Collector it needs to collect X*Y data points.
    %
    % Then we broadcast out {CollectorPid, report} to each of the cells, to tell
    % them to self-report their {index, life} to the Collector.
    %
    % Then we switch over to waiting for the Collector to ping us back.
    report   -> C_Ref = collector:init(self(), nil, S#state.size),
                [ C ! {C_Ref, report  } || C <- S#state.cells],
                waiting(S);
    % Similarly, if we need to tell all the cells to check their neighbors, we
    % create a Latch which knows to report back to the Board when it's done. We
    % tell the Latch it needs to collect X*Y pings.
    %
    % Then we broadcast out {LatchPid, check_ns} to each of the cells, to tell
    % them to check their neighbors and ping the Latch when they're done.
    %
    % Then we switch over to waiting for the Latch to ping us back.
    check_ns -> L_Ref = latch:init(self(), S#state.size),
                [ C ! {L_Ref, check_ns} || C <- S#state.cells],
                waiting(S);
    % FInally, if we need to tell all the cells to alter their states, we create
    % a Latch and broadcast our message just as we did above.
    alter_st -> L_Ref = latch:init(self(), S#state.size),
                [ C ! {L_Ref, alter_st} || C <- S#state.cells],
                waiting(S);
    _        -> erlang:error(weird_stage)
  end.

% When our board is waiting for a report, it expects a payload like 
% {data, Meta, Data}. It collects the Data, uses it to draw the gameboard at
% that frame, and then calls action() at the next stage, check_ns, and with one
% fewer frame left to go.
waiting(S = #state{stage=report}) ->
  receive {data, _, Data} -> out:writeData(S#state.frames, S#state.x, Data),
                             io:format("~w frames left.~n",[S#state.frames]),
                             action(S#state{ stage=check_ns
                                           , frames=(S#state.frames-1)
                                           })
  after 1000 -> erlang:error(board_timeout)
  end;

% When our board is waiting for anything else, it expects a simple pong.
% Depending on the current stage, it will know to switch over to the next stage
% in the cycle.
waiting(S) ->
  receive pong -> 
    case S#state.stage of check_ns -> action(S#state{stage=alter_st});
                          alter_st -> action(S#state{stage=report  })
    end
  after 1000 -> erlang:error(board_timeout)
  end.
