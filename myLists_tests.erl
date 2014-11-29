-module(myLists_tests).
-include_lib("eunit/include/eunit.hrl").

set_get_test_() ->
    M = myLists:getMap1(),
    N = myLists:set_(3,1,"foo",M),
    X = myLists:get_(3,1,N),
    [?_assertEqual("foo",X)].

intersection_test_() ->
    L1 = [3,1,5,2,14,7],
    L2 = [8,12,1,5,13,3,7],
    L3 = myLists:difference(L1,L2),
    [?_assertEqual([2,14],L3)].
