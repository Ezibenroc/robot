-module(ui).
-export([start/0,allNames/0,listen_loop/1,printScore/0,terminate/0]).

-define(ROBOT_NODE, 'bob@foo.bar').
-define(COOKIE, 'asimov').

flood() ->
    X=net_adm:ping(?ROBOT_NODE),
    case X of
        pong -> io:fwrite("Connected to UI.\n");
        _ -> timer:sleep(100), flood()
    end.

print(L) ->
    lists:map(fun(X) -> io:fwrite("\t~w\n",[X]) end, L).

listen(State) ->
    receive
        {robotList,L} -> io:fwrite("Robot list:\n"), print(L), State;
        {someonescored,Student,Score} -> [{Student,Score}|State];
        printscore -> print(State), State;
        terminationsuccess -> io:fwrite("Arbiter and robots successfully terminated.\n"), State;
        {terminationfailure,L} -> io:fwrite("Some robots did not respond to termination request:\n"),
            print(L), State;
        X -> io:fwrite("Received unknown message: ~w\n",[X]), State
    end.

listen_loop(State) ->
    Y=listen(State),
    listen_loop(Y).

allNames() ->
    { arbiter, ?ROBOT_NODE } ! {arbiterRequest,self(),info,[ui,robots]}.

printScore() ->
    listener ! printscore.

terminate() ->
    { arbiter, ?ROBOT_NODE } ! {arbiterRequest,self(),exit,[]}.

% Function to start the UI
start() ->
    erlang:set_cookie(node(),?COOKIE),
    flood(),
    register(listener,spawn(ui,listen_loop,[[]])).
