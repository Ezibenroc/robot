-module(myArbiter_tests).
-include_lib("eunit/include/eunit.hrl").

move_test_() ->
    myArbiter:start(myLists:getState1()),
    arbiter ! {arbiterRequest,self(),action,[move,{1,1},{0,1}]},
    receive OutMap1 -> OutMap1 end,
    arbiter ! {arbiterRequest,self(),action,[move,{1,1},{1,0}]},
    receive OutMap2 -> OutMap2 end,
    arbiter ! {arbiterRequest,self(),action,[move,{9,9},{10,9}]},
    receive OutMap3 -> OutMap3 end,
    arbiter ! {arbiterRequest,self(),action,[move,{9,9},{9,10}]},
    receive OutMap4 -> OutMap4 end,
    arbiter ! {arbiterRequest,self(),action,[move,{1,1},{1,2}]},
    receive X1 -> X1 end,
    arbiter ! {arbiterRequest,self(),action,[move,{5,1},{1,2}]},
    receive X2 -> X2 end,
    arbiter ! {arbiterRequest,self(),action,[move,{1,2},{1,3}]},
    receive X3 -> X3 end,
    arbiter ! {arbiterRequest,self(),action,[move,{1,2},{1,5}]},
    receive X4 -> X4 end,
    arbiter ! {arbiterRequest,self(),action,[move,{9,9},{8,8}]},
    receive X5 -> X5 end,
    arbiter ! {arbiterRequest,self(),info,[debug]},
    receive S1 -> S1 end,
    [?_assertEqual(blocked, OutMap1),
    ?_assertEqual(blocked, OutMap2),
    ?_assertEqual(blocked, OutMap3),
    ?_assertEqual(blocked, OutMap4),
    ?_assertEqual(ok, X1),
    ?_assertEqual(invalid, X2),
    ?_assertEqual(blocked, X3),
    ?_assertEqual(invalid, X4),
    ?_assertEqual(ok, X5),
    ?_assertEqual({" ",0}, myLists:get_(1,1,element(3,S1))),
    ?_assertEqual({"r",0}, myLists:get_(1,2,element(3,S1)))].

enter_test_() ->
    myArbiter:start(myLists:getState1()),
    arbiter ! {arbiterRequest,self(),action,[enter,3,foo]},
    receive X1 -> X1 end,
    arbiter ! {arbiterRequest,self(),action,[enter,1,foo]},
    receive X2 -> X2 end,
    arbiter ! {arbiterRequest,self(),action,[enter,1,foo]},
    receive X3 -> X3 end,
    arbiter ! {arbiterRequest,self(),info,[debug]},
    receive S1 -> S1 end,
    [?_assertEqual(invalid, X1),
    ?_assertEqual(ok, X2),
    ?_assertEqual(blocked, X3),
    ?_assertEqual({"r",0}, myLists:get_(3,3,element(3,S1)))].
