-module(ui).
-export([start/0,allNames/0]).

-define(ROBOT_NODE, 'bob@foo.bar').
-define(UI_NAME, user_interface).
-define(COOKIE, 'asimov').

flood() ->
    X=net_adm:ping(?ROBOT_NODE),
    case X of
        pong -> io:fwrite("Connected to UI.\n");
        _ -> timer:sleep(100), flood()
    end.

allNames() ->
    { arbiter, ?ROBOT_NODE } ! {arbiterRequest,self(),info,[?UI_NAME,robots]},
    receive
        L -> L
    after 100 -> io:fwrite(standard_error,"Arbiter does not respond.\n",[])
    end.

% Function to start the UI
start() ->
    erlang:set_cookie(node(),?COOKIE),
    flood(),
    register(?UI_NAME,self()).
