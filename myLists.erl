-module(myLists).
-export([set/3]).

% Set the N-th cell of the list to X
% Index begin by 1
set(1,X,[_|T]) -> [X|T];
set(N,X,[H|T]) -> [H|set(N-1,X,T)].
