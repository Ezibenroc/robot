% Main module of the tests. Perform all existing tests.

-module(allTests).
-export([test/0]).

test() ->
    myLists:test(),
    myArbiter:test(),
    myRobot:test().
