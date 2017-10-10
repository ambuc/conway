-module(collector).
-compile(export_all).

% Starts a collector reporting to Ref, waiting for N reports.
% Optionally bundled with Metadata of any type, as a secondary payload.
init(Ref, Meta, N) -> spawn(?MODULE, loop, [Ref, Meta, [], N]).
 
% Ends a collector reporting to Ref.
% Sends a payload like {data, Meta, Data}
loop(Ref, Meta, Data, 0) -> Ref ! {data, Meta, Data};

% Recieves a payload and returns a collector waiting for N-1 more payloads.
loop(Ref, Meta, Data, X) ->
  receive D    -> loop(Ref, Meta, [ D | Data ], X-1)
  after   1000 -> erlang:error(collector_timeout)
  end.
