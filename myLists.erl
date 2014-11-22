-module(myLists).
-include_lib("eunit/include/eunit.hrl").
-export([set_/4,get_/3,print/1,getMap1/0,getState1/0,getState2/0,getEntryPoints/0,getExitPoints/0]).

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

% Transform the given character
charTransform(C)->
    case C of
        "o" -> " ";
        X -> X
    end.

% Print the given map.
printMap([]) -> nope;
printMap([H|T]) -> 
    io:fwrite("|~s|\n",[lists:foldl(fun(X,Acc) -> string:concat(string:concat(Acc," "),charTransform(X)) end, "", H)]), 
    printMap(T).
print([]) ->
    io:fwrite("EMPTY MAP\n");
print([H|T]) ->
    io:fwrite(" ~s\n",[lists:foldl(fun(_,Acc) -> string:concat(string:concat(Acc,"-"),"-") end, "", H)]),
    printMap([H|T]),
    io:fwrite(" ~s\n",[lists:foldl(fun(_,Acc) -> string:concat(string:concat(Acc,"-"),"-") end, "", H)]).


getMap1() ->
    [
    ["r"," ","x"," "," "," "," "," "," "],
    [" "," ","x","x"," "," "," "," "," "],
    [" "," "," ","x"," "," "," "," "," "],
    [" "," "," ","x","x"," "," "," "," "],
    [" "," "," ","x","x"," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," ","r"]
    ].

getEntryPoints() -> [{3,3},{8,2}].
getExitPoints() -> [{3,6}].

getState1() ->
    {getEntryPoints(),getExitPoints(),getMap1()}. % Entry points, Exit points, Map

getMap2() ->
    [
    [" "," ","x"," "," "," "," "," "," "],
    [" "," ","x","x"," "," "," "," "," "],
    [" "," "," ","x"," "," "," "," "," "],
    [" "," "," ","x","x"," "," "," "," "],
    [" "," "," ","x","x"," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "],
    [" "," "," "," "," "," "," "," "," "]
    ].
getState2() ->
    {getEntryPoints(),getExitPoints(),getMap2()}. % Entry points, Exit points, Map
