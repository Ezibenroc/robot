-module(myRobot).
-import(robotUtils,[multiSend/2]).
-export([noMessage/4,mainRobot/4]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).
-define(STUDENT,tom).

nextCell({X,Y},Ori) ->
    case Ori of
        northeast ->    {{X+1,Y},north};
        north ->        {{X+1,Y-1},northwest};
        northwest ->    {{X,Y-1},west};
        west ->         {{X-1,Y-1},southwest};
        southwest ->    {{X-1,Y},south};
        south ->        {{X-1,Y+1},southeast};
        southeast ->    {{X,Y+1},east};
        east ->         {{X,Y},finish};
        _ ->            {{X+1,Y+1},northeast}
    end.

% Memory format: {X,Y},TerminationRequester
%    {X,Y} is the position

% When one robot receive {terminate_request}, he handle all its last messages, then send an ack and quit.

% Function called to start a robot.
mainRobot(State,{X,Y},TerminationRequester,ID) ->
%    ?debugVal(State),
    receive
        {PID,terminate_request} -> mainRobot(terminate,{X,Y},[PID|TerminationRequester],ID);
        {entries,ListEntries} -> handleEntries(State,{X,Y},TerminationRequester,ID,ListEntries);
        ok -> handleOk(State,{X,Y},TerminationRequester,ID);
        invalid -> handleInvalid(State,{X,Y},TerminationRequester,ID);
        blocked -> handleBlocked(State,{X,Y},TerminationRequester,ID);
        {Content,Message} -> handleInfo(State,{X,Y},TerminationRequester,ID,Content,Message);
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
        normal -> searchGold({normal,[]},{X,Y},TerminationRequester,ID);
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
        {arbiterRequest,enter,Entry,ListEntries} -> 
            Pos = lists:nth(Entry,ListEntries),
%            ?debugMsg("ENTER"),
            mainRobot(normal,Pos,TerminationRequester,ID);
        {arbiterRequest,move,Pos,_} ->
%            ?debugMsg("MOVE"),
            mainRobot(normal,Pos,TerminationRequester,ID);
        {arbiterRequest,collect,_,Ori,EmptyCells} ->
%            ?debugMsg("COLLECT"),
            searchGold({Ori,EmptyCells},{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleInvalid(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,_,_} ->
            ?debugMsg("Received invalid in state enter."),
            mainRobot(init,{X,Y},TerminationRequester,ID);
        {arbiterRequest,move,_,_} ->
            ?debugMsg("Received invalid in state move."),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        {arbiterRequest,collect,_,_} ->
            ?debugMsg("Received invalid in state collect."),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        {arbiterRequest,analyze,_,_,_} ->
            ?debugMsg("Received invalid in state analyze."),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        _ -> ?debugMsg("Received invalid in unknown state."),
            mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleBlocked(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,Entry,_} -> 
            arbiter ! {arbiterRequest,self(),action,[enter,Entry,ID]},
            mainRobot(State,{X,Y},TerminationRequester,ID);
        {arbiterRequest,move,_,_} ->
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

randomWalk(_,{X,Y},TerminationRequester,ID) ->
    NewPos={X+trunc(random:uniform()*3)-1,Y+trunc(random:uniform()*3)-1},
    arbiter ! {arbiterRequest,self(),info,[analyze,{X,Y},NewPos]},
    mainRobot({arbiterRequest,analyze,NewPos,unknown},{X,Y},TerminationRequester,ID).

searchGold({Ori,EmptyCells},{X,Y},TerminationRequester,ID) ->
    case Ori of
        finish -> 
%        ?debugFmt("Emptycells= ~w",[EmptyCells]),
            case EmptyCells of
                [] -> mainRobot(normal,{X,Y},TerminationRequester,ID);
                _ ->
                    Next=trunc(random:uniform()*length(EmptyCells))+1,
                    NextPos = lists:nth(Next,EmptyCells),
                    arbiter ! {arbiterRequest,self(),action,[move,{X,Y},NextPos]},
                    mainRobot({arbiterRequest,move,NextPos},{X,Y},TerminationRequester,ID)
                end;
        _ ->
%            ?debugFmt("Emptycells= ~w",[EmptyCells]),
            {NewPos,NewOri} = nextCell({X,Y},Ori),
            arbiter ! {arbiterRequest,self(),info,[analyze,{X,Y},NewPos]},
            mainRobot({arbiterRequest,analyze,NewPos,NewOri,EmptyCells},{X,Y},TerminationRequester,ID)
    end.

handleInfo(State,{X,Y},TerminationRequester,ID,Content,_) ->
    case State of
        {arbiterRequest,analyze,Pos,Ori,EmptyCells} ->
            case Content of
                gold -> arbiter ! {arbiterRequest,self(),action,[collect,{X,Y},Pos,?STUDENT]},
                    mainRobot({arbiterRequest,collect,Pos,Ori,[Pos|EmptyCells]},{X,Y},TerminationRequester,ID);
                empty ->
                    searchGold({Ori,[Pos|EmptyCells]},{X,Y},TerminationRequester,ID);
                _ -> 
                    searchGold({Ori,EmptyCells},{X,Y},TerminationRequester,ID)
            end;
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.
