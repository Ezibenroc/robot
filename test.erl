% Tests of the given packages.
-module(test).
-import(factory,[spawnFactory/3]).
-export([hello/1, spawn_hello/0]).

% Hello world function
hello(X) ->
  io:fwrite("Hello world, I am robot ~s.\n",[X]).
  
% Create a factory and spawn 10 helloworld robots
spawn_hello() ->
  spawnFactory(test,hello,[]),
  factory ! {spawn,3,self()},
  receive
    {spawned,ListOfProcessNames} -> ListOfProcessNames;
    _ -> error
  end.
  
