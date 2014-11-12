-module(myRobot).
-import(robotUtils,[multiSend/2]).
-export([noMessage/4,mainRobot/4]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).

% Memory format: {X,Y},TerminationRequester
%    {X,Y} is the position

% When one robot receive {terminate_request}, he handle all its last messages, then send an ack and quit.

% Function called to start a robot.
mainRobot(State,{X,Y},TerminationRequester,ID) ->
    receive
        {PID,terminate_request} -> mainRobot(terminate,{X,Y},[PID|TerminationRequester],ID);
        {entries,ListEntries} -> handleEntries(State,{X,Y},TerminationRequester,ID,ListEntries);
        ok -> handleOk(State,{X,Y},TerminationRequester,ID);
        invalid -> handleInvalid(State,{X,Y},TerminationRequester,ID);
        blocked -> handleBlocked(State,{X,Y},TerminationRequester,ID);
        X -> io:fwrite("Robot ~w received unknown message: ~w\n",[ID,X])
    after ?TIME_REC ->
        case State of
            terminate -> robotUtils:multiSend(TerminationRequester,{self(),ackTerminate}), terminated;
            _ -> noMessage(State,{X,Y},TerminationRequester,ID)
        end
    end.

% Function called when there is no message to read, and process do not need to terminate.
noMessage(State,{X,Y},TerminationRequester,ID) ->
    case State of
        init -> arbiter ! {arbiterRequest,self(),info,[entry]}, % ask the coordinates of the entry
            mainRobot(State,{X,Y},TerminationRequester,ID) ;
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

% Function called for the reception of entries
handleEntries(State,{X,Y},TerminationRequester,ID,ListEntries) ->
    case State of
        init -> random:seed(now()), 
            Entry=trunc(random:uniform()*length(ListEntries))+1,
            arbiter ! {arbiterRequest,self(),action,[enter,Entry,ID]}, % WTF is this bug ?
            mainRobot({arbiterRequest,enter,Entry,ListEntries},{X,Y},TerminationRequester,ID) ;
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleOk(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,Entry,ListEntries} -> Pos = lists:nth(Entry,ListEntries),
            mainRobot(normal,Pos,TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleInvalid(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,_,_} -> io:fwrite("Unexpected answer.\n"),
            mainRobot(init,{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleBlocked(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,Entry,_} -> arbiter ! {arbiterRequest,self(),action,[enter,Entry,ID]},
            mainRobot(State,{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.
