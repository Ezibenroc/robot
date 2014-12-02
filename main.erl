-module(main).
-export([start/0]).

-define(UI_NODE, 'alice@abc.def').
-define(COOKIE, 'asimov').

flood() ->
    X=net_adm:ping(?UI_NODE),
    case X of
        pong -> io:fwrite("Connected to MAIN.\n");
        _ -> timer:sleep(10), flood()
    end.

% Function to start everything
start() ->
    N = node(),
    case N of
        'nonode@nohost' -> nope;
        _ ->    erlang:set_cookie(node(),?COOKIE),
                flood()
    end,
    myArbiter:start(myLists:getState3()),
    myRobot:spawnFactory(),
    factory ! {spawn,10,self()}.
