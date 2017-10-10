-module(latch).
-compile(export_all).

% Starts a latch reporting to Ref, waiting for N pings.
init(Ref, N) -> spawn(?MODULE, loop, [Ref, N]).

% Ends a latch reporting to Ref.
loop(Ref, 0) -> Ref ! pong;

% Recieves a ping and returns a latch waiting for N-1 pings.
loop(Ref, X) ->
  receive ping -> loop(Ref, X-1)
  after   1000 -> erlang:error(latch_timeout)
  end.
