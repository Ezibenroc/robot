-module(myLists).
-include_lib("eunit/include/eunit.hrl").
-export([set_/4,get_/3,print/1,getMap/0,getState/0]).

% set_ the N-th cell of the list to X
% Index begin by 1
set_(1,X,[_|T]) -> [X|T];
set_(N,X,[H|T]) -> [H|set_(N-1,X,T)].

%% The map is stored as a list of lists.

% set_ the cell (N,M) of the given map to X.
set_(1,M,X,[H|T]) -> [set_(M,X,H)|T];
set_(N,M,X,[H|T]) -> [H|set_(N-1,M,X,T)].

% Get the cell (N,M) of the given map.
get_(N,M,L) -> lists:nth(M,lists:nth(N,L)).

% Print the given map.
print([]) -> io:fwrite("\n");
print([H|T]) -> 
    io:fwrite("~s\n",[lists:foldl(fun(X,Acc) -> string:concat(string:concat(Acc," "),X) end, "", H)]), 
    print(T).
    
getMap() ->
    [
    ["r","o","x","o","o","o","o","o","o"],
    ["o","o","x","x","o","o","o","o","o"],
    ["o","o","o","x","o","o","o","o","o"],
    ["o","o","o","x","x","o","o","o","o"],
    ["o","o","o","x","x","o","o","o","o"],
    ["o","o","o","o","o","o","o","o","o"],
    ["o","o","o","o","o","o","o","o","o"],
    ["o","o","o","o","o","o","o","o","o"],
    ["o","o","o","o","o","o","o","o","r"]
    ].

getState() ->
    {[{1,1}],[{3,3}],getMap()}. % Entry points, Exit points, Map
