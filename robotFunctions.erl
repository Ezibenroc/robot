-module(robotFunctions).
-export([electLeader/0, userInterface/0]).
-import(lists,[sort/1]).

% General command format of messages:
% { Pid, Nature, Parameters }
% Pid is a PID. 
% Nature is an atom.
% Parameters is a list.

% Leader election
% We choose the process with the smallest name, and which answer.
electLeader() ->
  l=robotUtils:allNames(),
  sort(l),
  chooseLeader(l).

chooseLeader([])->
  io:fwrite("Error, no leader to elect.\n");
chooseLeader([H|T])->
  H ! { self(), youareleader, [] },
  receive { PID, iamleader, [] } -> PID after 1000 -> chooseLeader(T) end.
  


% User interface
userInterface() ->
  A=io:get_line(">>"),
  case A of
    elect -> electLeader(), userInterface();
    X -> io:fwrite("Unknown command: ~s\n",[X]), userInterface()
  end.
