% Main module to launch the arbiter, the factory, and start robots.
-module(main).
-export([start/0,superarbiter_loop/0]).

-define(UI_NODE, 'alice@abc.def').
-define(COOKIE, 'asimov').

% Connection with the UI at the start of the module.
flood() ->
    X=net_adm:ping(?UI_NODE),
    case X of
        pong -> io:fwrite(standard_error,"Connected to UI.\n",[]);
        _ -> timer:sleep(109), flood()
    end.

% Dummy function to emulate the superarbiter, for test purposes.
superarbiter_listen() ->
    receive
        {score,Student,Value,Pid} -> io:fwrite(standard_error,"Superarbiter:\tstudent ~w scored ~w\n",[Student,Value]), Pid ! saok;
        X -> io:fwrite(standard_error,"Superarbiter:\treceived unknown message ~w\n",[X])
    end.

superarbiter_loop() ->
    superarbiter_listen(),
    superarbiter_loop().

% Function to start everything
start() ->
    N = node(),
    case N of
        'nonode@nohost' -> nope;
        _ ->    erlang:set_cookie(node(),?COOKIE),
                flood()
    end,
    % Uncomment to test the superarbiter I coded.
%   register(superarbiter,spawn(main,superarbiter_loop,[])),
    myArbiter:start(myLists:getState3()),
    myRobot:spawnFactory(),
    factory ! {spawn,10,self()}.
