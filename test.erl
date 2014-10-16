% Tests of the given packages.
-module(test).
-import(factory,[spawnFactory/3]).
-export([hello/1,spawn_hello/0,first_robot/1,start/3]).

% Hello world function
hello(X) ->
  io:fwrite("Hello world, I am robot ~s.\n",[X]).
  
% Create a factory and spawn 10 helloworld robots
spawn_hello() ->
  spawnFactory(test,hello,[]),
  factory ! {spawn,10,self()},
  receive
    {spawned,ListOfProcessNames} -> ListOfProcessNames;
    _ -> error
  end.

% Robot from question 2, receive messages (and print them). Exit when receive 'quit'.
first_robot(X) ->
  receive
    quit -> quit ;
    Mess -> io:fwrite("Received: ~s\n",[Mess]), first_robot(X)
  end.
  
% Spawn a factory and 10 robots with the given module, function and arguments.
% Return the list of the process names.
% The function need to be in the export list of the module.
start(Module,Function,Args) ->
  spawnFactory(Module,Function,Args),
  factory ! {spawn,10,self()},
  receive
    {spawned,ListOfProcessNames} -> ListOfProcessNames;
    _ -> error
  end.
