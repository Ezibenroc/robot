-module(myLists_tests).
-include_lib("eunit/include/eunit.hrl").

set_get_test_() ->
    M = myLists:getMap(),
    N = myLists:set_(3,1,"foo",M),
    X = myLists:get_(3,1,N),
    [?_assert(X =:= "foo")].
