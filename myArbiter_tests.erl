-module(myArbiter_tests).
-include_lib("eunit/include/eunit.hrl").

move_test_() ->
    myArbiter:start(),
    arbiter ! {arbiterRequest,self(),action,[move,[1,1],[1,2]]},
    receive X1 -> X1 end,
    arbiter ! {arbiterRequest,self(),action,[move,[5,1],[1,2]]},
    receive X2 -> X2 end,
    arbiter ! {arbiterRequest,self(),action,[move,[1,2],[1,3]]},
    receive X3 -> X3 end,
    arbiter ! {arbiterRequest,self(),action,[move,[1,2],[1,5]]},
    receive X4 -> X4 end,
    arbiter ! {arbiterRequest,self(),info,[]},
    receive S1 -> S1 end,
    [?_assert(X1 =:= ok),
    ?_assert(X2 =:= invalid),
    ?_assert(X3 =:= blocked),
    ?_assert(X4 =:= invalid),
    ?_assert(myLists:get_(1,1,S1) =:= "o"),
    ?_assert(myLists:get_(1,2,S1) =:= "r")].
