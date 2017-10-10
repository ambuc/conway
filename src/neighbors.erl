-module(neighbors).
-compile(export_all).

% Asking for the neighboring PIDs of a cell requires calculating the indices of
% the neighboring cells of some index in question. We look that up with getNs()
% and then map an index lookup (lists:nth()) over the list of neighboring
% indices.
getNPids(I,W,H,Cs) -> lists:map( fun(N) -> lists:nth(N,Cs) end , getNs(I,W,H)).

% To calculate the neighboring indices, we have to check eight edge cases; the
% four corner and the four edges. Otherwise the cell can be assumed to be
% surrounded.
getNs(I,W,H) ->
  UL = 1,                              UR = W,
  LL = W*H - W + 1,                    LR = W*H,
  UE = (I>1) and (I<UR),               LE = I rem W == 1,
  RE = I rem W == 0,                   BE = (I>LL) and (I<LR),
  if I =:= UL -> [                    I+1,      I+W,I+W+1]; % is upper-left?
     I =:= UR -> [                I-1,    I+W-1,I+W      ]; % is upper-right?
     I =:= LL -> [      I-W,I-W+1,    I+1                ]; % is lower-left?
     I =:= LR -> [I-W-1,I-W,      I-1                    ]; % is lower-right?
     UE       -> [                I-1,I+1,I+W-1,I+W,I+W+1]; % is on upper edge?
     LE       -> [      I-W,I-W+1,    I+1,      I+W,I+W+1]; % is on left edge?
     RE       -> [I-W-1,I-W,      I-1,    I+W-1,I+W      ]; % is on right edge?
     BE       -> [I-W-1,I-W,I-W+1,I-1,I+1                ]; % is on bottom edge?
     true     -> [I-W-1,I-W,I-W+1,I-1,I+1,I+W-1,I+W,I+W+1]  % default case
  end.

