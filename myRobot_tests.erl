-module(myRobot_tests).
-include_lib("eunit/include/eunit.hrl").

terminateAck_receive() ->
    receive
        {_,ackTerminate} -> terminateAck_receive()+1
    after 100 -> 0 end.

% For some unknown reason, eunit perform the test several times in parallel.
% For instance, if one want to spawn X robots in a test, there might be 2X or 3X robots...
termination_test_() ->
    factory:spawnFactory(myRobot, mainRobot, [[1,1],false,[]]),
    factory ! {spawn,10,self()},
    ?debugVal(self()),
    receive {spawned, ListRobot} -> ListRobot end,
    ListRobot2 = robotUtils:allNames(),
    robotUtils:broadcast({self(),terminate_request}),
    NbTerminate = terminateAck_receive(),
    ListRobot3 = robotUtils:allNames(),
    ListLength = length(ListRobot2),
    [?_assertNot(ListRobot2=:=[]),
    ?_assertEqual(ListRobot3,[]),
    ?_assertEqual(ListLength,NbTerminate)].
