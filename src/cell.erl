-module(cell).
-compile(export_all).

-record(state, {index, life=0, desire, npids=[]}).

% Starts a cell with knowledge of its own index and starting state.
init(Index, Life) -> spawn(?MODULE, loop, [ #state{index=Index, life=Life} ]).

% Recieves a signal, either...
loop(S) ->
  receive
    % ... a moore neighbor space,
    {moore, NPids}   -> loop(S#state{npids=NPids});
    % ... a request to check their neighbors' states, decide their desired
    % state, and ping back to a latch L when done,
    {L, check_ns}    -> Ref = collector:init(self(),L,length(S#state.npids)),
                        [ NPid ! {Ref, alive} || NPid <- S#state.npids ],
                        loop(S);
    % ... a payload from the above collector, allowing them to decide their
    % desired state, and ping a latch on receipt,
    {data, L, NData} -> L ! ping, 
                        loop(S#state{desire=getDesire(S#state.life,NData)});
    % ... a request from the board to ping a latch and flip to their new state,
    {L, alter_st}    -> L ! ping, 
                        loop(S#state{life=(S#state.desire)});
    % ... a request to report their {index,life},
    {C, report}      -> C ! {S#state.index, S#state.life}, 
                        loop(S);
    % ... a request to report their life,
    {Ref, alive}     -> Ref ! S#state.life,
                        loop(S);
    % ... or a request to die.
    die              -> ok;
    _                -> erlang:error(weird_input)
  after 10000        -> erlang:error(cell_timeout)
  end.

% Takes a current state and data about the states of their neighbors, and
% decides whether to live or die.
getDesire(Curr, NData) ->
  decide(Curr, length(lists:filter(fun(X) -> X =:= 1 end, NData))).

% Encodes the GOL rules.
% - Any live cell with fewer than two live neighbours dies, as if caused by 
%   underpopulation.  
% - Any live cell with two or three live neighbours lives on to the next 
%   generation.
% - Any live cell with more than three live neighbours dies, as if by
%   overpopulation. 
% - Any dead cell with exactly three live neighbours becomes a live cell, as if
%   by reproduction.
% Formally, this might be encoded as "B3/S23".
decide(0,3) -> 1;
decide(0,_) -> 0;
decide(1,2) -> 1;
decide(1,3) -> 1;
decide(1,_) -> 0.
