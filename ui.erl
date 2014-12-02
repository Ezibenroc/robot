-module(ui).
-export([start/0,allNames/0,listen_loop/0]).

-define(ROBOT_NODE, 'bob@foo.bar').
-define(COOKIE, 'asimov').

flood() ->
    X=net_adm:ping(?ROBOT_NODE),
    case X of
        pong -> io:fwrite("Connected to UI.\n");
        _ -> timer:sleep(100), flood()
    end.

listen() ->
    receive
        {robotList,L} -> io:fwrite("Robot list:\n~w\n",[L]);
        X -> io:fwrite("Received unknown message: ~w\n",[X])
    end.

listen_loop() ->
    listen(),
    listen_loop().

allNames() ->
    { arbiter, ?ROBOT_NODE } ! {arbiterRequest,self(),info,[ui,robots,node()]}.

% Function to start the UI
start() ->
    erlang:set_cookie(node(),?COOKIE),
    flood(),
    register(listener,spawn(ui,listen_loop,[])).
