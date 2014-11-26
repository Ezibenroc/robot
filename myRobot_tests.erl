-module(myRobot_tests).
-include_lib("eunit/include/eunit.hrl").

terminateAck_receive() ->
    receive
        {_,ackTerminate} -> terminateAck_receive()+1
    after 100 -> 0 end.

% For some unknown reason, eunit perform the test several times in parallel.
% For instance, if one want to spawn X robots in a test, there might be 2X or 3X robots...
termination_test_() ->
    myArbiter:start(myLists:getState2()),
    factory:spawnFactory(myRobot, mainRobot, [init,{-1,-1},[]]),
    factory ! {spawn,10,self()},
    ?debugVal(self()),
    receive {spawned, ListRobot} -> ListRobot end,
    ListRobot2 = robotUtils:allNames(),
    timer:sleep(1000),
    robotUtils:broadcast({self(),terminate_request}),
    NbTerminate = terminateAck_receive(),
    ListRobot3 = robotUtils:allNames(),
    ListLength = length(ListRobot2),
    [?_assertNot(ListRobot2=:=[]),
    ?_assertEqual(ListRobot3,[]),
    ?_assertEqual(ListLength,NbTerminate)].

exploreCell_test_() ->
    myArbiter:start(myLists:getState1()),
    OutMap1 = myRobot:exploreCell({1,1},{0,1}),
    OutMap2 = myRobot:exploreCell({1,1},{1,0}),
    OutMap3 = myRobot:exploreCell({9,9},{10,9}),
    OutMap4 = myRobot:exploreCell({9,9},{9,10}),
    Block = myRobot:exploreCell({1,4},{2,4}),
    Empty = myRobot:exploreCell({1,4},{1,5}),
    Gold = myRobot:exploreCell({9,9},{8,9}),
    Exit = myRobot:exploreCell({6,2},{6,3}),
    [?_assertNot(OutMap1),
    ?_assertNot(OutMap2),
    ?_assertNot(OutMap3),
    ?_assertNot(OutMap4),
    ?_assertNot(Block),
    ?_assert(Empty),
    ?_assert(Gold),
    ?_assertNot(Exit)].
