-module(myRobot).
-import(robotUtils,[multiSend/2]).
-export([noMessage/6,mainRobot/6]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).

% Memory format: [X,Y],TerminationRequester
%    [X,Y] is the position

% When one robot receive {terminate_request}, he handle all its last messages, then send an ack and quit.

% Function called to start a robot.
mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ->
    receive
        {PID,terminate_request} -> mainRobot(terminate,[X,Y],EntryPoints,ExitPoints,[PID|TerminationRequester],ID);
        ok -> handleOk(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID);
        invalid -> handleInvalid(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID);
        blocked -> handleBlocked(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID);
        X -> io:fwrite("Robot ~wreceived unknown message: ~w\n",[ID,X])
    after ?TIME_REC ->
        case State of
            terminate -> robotUtils:multiSend(TerminationRequester,{self(),ackTerminate}), terminated;
            _ -> noMessage(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID)
        end
    end.

% Function called when there is no message to read, and process do not need to terminate.
noMessage(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ->
    case State of
        init -> random:seed(now()), 
            Entry=trunc(random:uniform()*length(EntryPoints))+1,
            arbiter ! {arbiterRequest,self(),action,[enter,Entry,ID]}, % WTF is this bug ?
            mainRobot({arbiterRequest,enter,Entry},[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ;
        _ -> mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID)
    end.

handleOk(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,Entry} -> Pos = lists:nth(Entry,EntryPoints),
            mainRobot(normal,Pos,EntryPoints,ExitPoints,TerminationRequester,ID);
        _ -> mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID)
    end.

handleInvalid(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,_} -> io:fwrite("Unexpected answer.\n"),
            mainRobot(init,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID);
        _ -> mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID)
    end.

handleBlocked(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,Entry} -> arbiter ! {arbiterRequest,self(),action,[enter,Entry,ID]},
            mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID);
        _ -> mainRobot(State,[X,Y],EntryPoints,ExitPoints,TerminationRequester,ID)
    end.
