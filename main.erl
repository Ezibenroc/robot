-module(main).
-export([start/0]).

% Function to start everything
start() ->
    myArbiter:start(myLists:getState3()),
    myRobot:spawnFactory(),
    factory ! {spawn,10,self()}.
