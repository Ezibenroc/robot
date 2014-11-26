-module(main).
-export([start/0]).

% Function to start everything
start() ->
    myArbiter:start(myLists:getState4()),
    factory:spawnFactory(myRobot, mainRobot, [init,{-1,-1},[]]),
    factory ! {spawn,10,self()}.
