-module(out).
-compile(export_all).

% Ensures /tmp/conway exists and cleans it of all files at startup.
cleanOutputFolder() ->
    case file:make_dir("/tmp/conway") of
      ok -> ok;
      {error, eexist} -> ok;
      {error, _}-> erlang:error(idk)
    end
  , case file:list_dir("/tmp/conway") of
      {ok, Files} -> [ file:delete("/tmp/conway/"++F) || F <- Files];
      _ -> erlang:error(idk)
    end
  .

% Given some data of frame # N, on a game board of width W, we write the Data to
% the data_NNNN file row by row with writeRow(). Then we close the file and run
% the Haskell image renderer, which needs a source data file and a destination
% png to write. We also write out to /tmp/ghcirender.log for debugging purposes.
writeData(N,X,Data) ->
    {ok, Fh} = file:open(io_lib:format("/tmp/conway/data_~4..0B", [N]), write)
  , writeRow(Fh, X, lists:keysort(1,Data))
  , file:close(Fh)
  , os:cmd("renderFrame/render " 
          ++ io_lib:format("/tmp/conway/data_~4..0B ",[N])
          ++ io_lib:format("/tmp/conway/frame_~4..0B",[N])
          ++ " > /tmp/ghcirender.log"
          )
  .

% writeRow is really a bit hairy. It takes a list Data and takes a chunk W bits
% long of it; converts it to strings and concatenates them, and then writes them
% out newline-delimited to the filehandle Fh in question
writeRow(_ , _, []  ) -> ok;
writeRow(Fh, W, Data) ->
    {Row, Rest} = lists:split(W, Data)
  , file:write(Fh, lists:flatten( 
                    lists:map( fun({_,X}) -> io_lib:format("~p",[X]) end, Row )
                   )
              )
  , file:write(Fh, io_lib:format("~n",[]))
  , writeRow(Fh, W, Rest)
  .
