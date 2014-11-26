-module(myRobot).
-import(robotUtils,[multiSend/2]).
-export([noMessage/4,mainRobot/4,exploreCell/2,explore/1]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).
-define(STUDENT,tom).

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
        normal -> exploreAndMove(State,{X,Y},TerminationRequester,ID);
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
        {arbiterRequest,move,Pos} ->
%            ?debugMsg("MOVE"),
            mainRobot(normal,Pos,TerminationRequester,ID);
        {arbiterRequest,collect,_} ->
%            ?debugMsg("COLLECT"),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

handleInvalid(State,{X,Y},TerminationRequester,ID) ->
    case State of
        {arbiterRequest,enter,_,_} ->
            ?debugMsg("Received invalid in state enter."),
            mainRobot(init,{X,Y},TerminationRequester,ID);
        {arbiterRequest,move,_} ->
            ?debugMsg("Received invalid in state move."),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        {arbiterRequest,collect,_} ->
            ?debugMsg("Received invalid in state collect."),
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        {arbiterRequest,analyze,_} ->
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
        {arbiterRequest,move,_} ->
            mainRobot(normal,{X,Y},TerminationRequester,ID);
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

randomWalk(_,{X,Y},TerminationRequester,ID) ->
    NewPos={X+trunc(random:uniform()*3)-1,Y+trunc(random:uniform()*3)-1},
    arbiter ! {arbiterRequest,self(),info,[analyze,{X,Y},NewPos]},
    mainRobot({arbiterRequest,analyze,NewPos},{X,Y},TerminationRequester,ID).

handleInfo(State,{X,Y},TerminationRequester,ID,Content,_) ->
    case State of
        {arbiterRequest,analyze,Pos} ->
                case Content of
                    gold -> arbiter ! {arbiterRequest,self(),action,[collect,{X,Y},Pos,?STUDENT]},
                        mainRobot({arbiterRequest,collect,Pos},{X,Y},TerminationRequester,ID);
                    empty -> arbiter ! {arbiterRequest,self(),action,[move,{X,Y},Pos]},
                        mainRobot({arbiterRequest,move,Pos},{X,Y},TerminationRequester,ID);
                    _ -> mainRobot(normal,{X,Y},TerminationRequester,ID)
                end;
        _ -> mainRobot(State,{X,Y},TerminationRequester,ID)
    end.

% Explore the cell NewPos, assuming that the robot is located in Pos.
% If gold is found, then collect it.
% Return true if and only if a move to the destination is possible.
exploreCell(Pos,NewPos) ->
    arbiter ! {arbiterRequest,self(),info,[analyze,Pos,NewPos]},
    receive
        {empty,_} -> true;
        {gold,_} ->
            arbiter ! {arbiterRequest,self(),action,[collect,Pos,NewPos,?STUDENT]},
            receive
                ok -> true;
                invalid -> ?debugMsg("Error in exploreCell.")
            end;
        {blocked,_} -> false;
        {robotname,_} -> false;
        {exit,_} -> false
    end.

% Explore the given cell.
% If we can perform a move, return the singleton list [NewPos], otherwise return [].
exploreCellToList(Pos,NewPos) ->
    Move = exploreCell(Pos,NewPos),
    if
        Move -> [NewPos];
        true -> []
    end.

% Explore all the neighbours of the cell {X,Y}.
% Return the list of cells to which we can move.
explore({X,Y}) ->
    lists:append([
        exploreCellToList({X,Y},{X+1,Y}),
        exploreCellToList({X,Y},{X-1,Y}),
        exploreCellToList({X,Y},{X,Y+1}),
        exploreCellToList({X,Y},{X,Y-1}),
        exploreCellToList({X,Y},{X+1,Y+1}),
        exploreCellToList({X,Y},{X+1,Y-1}),
        exploreCellToList({X,Y},{X-1,Y+1}),
        exploreCellToList({X,Y},{X-1,Y-1})
        ]).

% Explore all the neighbours of the cell {X,Y}.
% Move randomly toward one empty cell.
exploreAndMove(State,{X,Y},TerminationRequester,ID) ->
    ListPos = explore({X,Y}),
    case ListPos of
        [] -> mainRobot(State,{X,Y},TerminationRequester,ID);
        _ -> 
            Next=trunc(random:uniform()*length(ListPos))+1,
            NextPos = lists:nth(Next,ListPos),
            arbiter ! {arbiterRequest,self(),action,[move,{X,Y},NextPos]},
            mainRobot({arbiterRequest,move,NextPos},{X,Y},TerminationRequester,ID)
    end.
