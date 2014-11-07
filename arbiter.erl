-module(arbiter).
-export([startArbiter/4,arbiterLoop/4, startArbiter/0, doNothing/4, debug/2, debug/3]).

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

arbiterLoop(HandleAction, HandleInfo, State, Debug) ->
    % receiving requests from users
    NewRequest = receive
        {arbiterRequest,Pid,Nature,Parameters} -> {Nature,Parameters}
    end,

    % debug message
    debug(Debug,"<arbiter> Arbiter received request ~w and is in state ~w.~n",[NewRequest,State]),

    % processing the request
    case NewRequest of
        {exit,_} -> done;
        {info, Params} -> HandleInfo(Pid, Params, State, Debug), arbiterLoop(HandleAction, HandleInfo, State, Debug);
        {debug,OnOff} -> arbiterLoop(HandleAction, HandleInfo, State,OnOff);
        {action, Params} -> arbiterLoop(HandleAction, HandleInfo, HandleAction(Pid, Params, State, Debug), Debug);
        _ -> debug(Debug,"<arbiter> Received badly formatted request ~w.~n",[NewRequest]), arbiterLoop(HandleAction,HandleInfo,State,Debug)
    end.


killArbiter() ->
    case whereis(arbiter) of
        undefined -> error;
        Pid -> exit(Pid,kill)
    end.

doNothing(Pid, _Parameters, State, Debug) -> debug(Debug,"<arbiter> Doing nothing. Pid is ~w. State is ~w.~n",[Pid,State]), State.
