-module(fog).
-compile(export_all).

% Fields a request for an array of random 0s and 1s exactly X*Y in length.
getFog(X,Y) -> randomPixel(X*Y,[]).
randomPixel(0,Ls) -> Ls;
% Uses random:uniform(), which gets a new seed on each loop.
randomPixel(N,Ls) -> randomPixel(N-1, [random:uniform(2)-1 | Ls]).
