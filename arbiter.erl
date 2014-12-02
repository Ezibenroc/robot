% Arbiter module. Basic arbiter functions.

-module(arbiter).
-export([startArbiter/4,arbiterLoop/4, startArbiter/0, doNothing/4, debug/2, debug/3]).
-define(UI_NODE, 'alice@abc.def').

startArbiter() -> startArbiter(fun doNothing/4, fun doNothing/4, [], true).
startArbiter(HandleAction, HandleInfo, InitState, Debug) ->
    case whereis(arbiter) of
        undefined -> register(arbiter,spawn(arbiter,arbiterLoop,[HandleAction, HandleInfo, InitState, Debug]));
        _ -> io:fwrite("<spawnArbiter> Killing old arbiter...~n"), killArbiter(), startArbiter(HandleAction, HandleInfo, InitState, Debug)
    end.

debug(OnOff,Message) -> debug(OnOff,Message,[]).
debug(OnOff,Message,Args) ->
    if
        OnOff -> io:fwrite(Message,Args);
        true -> nope
    end.

terminateAck(L) ->
    receive
        {PID,ackTerminate} ->
            terminateAck(lists:filter(fun(X) -> X =/= PID end, L))
    after 100 -> L end.

arbiterLoop(HandleAction, HandleInfo, State, Debug) ->
    io:fwrite("~w\n",[calendar:local_time()]),
    % receiving requests from users
    NewRequest = receive
        {arbiterRequest,Pid,Nature,Parameters} -> {Nature,Parameters}
    end,

    % debug message
    debug(Debug,"<arbiter> Arbiter received request ~w and is in state ~w.~n",[NewRequest,State]),
    myLists:print(element(3,State)),
    % processing the request
    case NewRequest of
        % EXIT REQUEST (delay of 2000ms)
        {exit,_} ->
            io:fwrite(standard_error,"UI request:\t~w\n",[NewRequest]),
            ListRobot = robotUtils:allPids(),
            robotUtils:broadcast({self(),terminate_request}),
            timer:send_after(2000,self(),{arbiterRequest,self(),handleexit,ListRobot}),
            arbiterLoop(HandleAction, HandleInfo, State, Debug);
        % HANDLING OF THE EXIT REQUEST
        {handleexit,ListRobot} ->
            L = terminateAck(ListRobot),
            case L of
                [] -> io:fwrite(standard_error,"Arbiter:\tall robots terminated, stop.\n",[]),
                    { listener, ?UI_NODE } ! terminationsuccess;
                _ -> io:fwrite(standard_error,"Arbiter:\tthe following robots did not respond to termination request:\n",[]),
                    lists:map(fun(X) -> io:fwrite(standard_error,"\t~w\n",[X]) end, L),
                    { listener, ?UI_NODE } ! {terminationfailure,L}
            end;
        % INFO REQUEST
        {info, Params} -> HandleInfo(Pid, Params, State, Debug), arbiterLoop(HandleAction, HandleInfo, State, Debug);
        % DEBUG REQUEST
        {debug,OnOff} -> arbiterLoop(HandleAction, HandleInfo, State,OnOff);
        % ACTION REQUEST
        {action, Params} -> arbiterLoop(HandleAction, HandleInfo, HandleAction(Pid, Params, State, Debug), Debug);
        % UNKNOWN REQUEST
        _ -> debug(Debug,"<arbiter> Received badly formatted request ~w.~n",[NewRequest]), arbiterLoop(HandleAction,HandleInfo,State,Debug)
    end.


killArbiter() ->
    case whereis(arbiter) of
        undefined -> error;
        Pid -> exit(Pid,kill)
    end.

doNothing(Pid, _Parameters, State, Debug) -> debug(Debug,"<arbiter> Doing nothing. Pid is ~w. State is ~w.~n",[Pid,State]), State.
