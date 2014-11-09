-module(myRobot).
-import(robotUtils,[multiSend/2]).
-export([mainRobot/6]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).

% Memory format: [X,Y],Termination,TerminationRequester
%    [X,Y] is the position

% When one robot receive {terminate_request}, he handle all its last messages, then send an ack and quit.

% Function called to start a robot.
mainRobot([X,Y],EntryPoints,ExitPoints,Termination,TerminationRequester,ID) ->
    receive
        {PID,terminate_request} -> mainRobot([X,Y],EntryPoints,ExitPoints,true,[PID|TerminationRequester],ID)
    after ?TIME_REC ->
        if
            Termination -> robotUtils:multiSend(TerminationRequester,{self(),ackTerminate}), terminated;
            true -> noMessage([X,Y],EntryPoints,ExitPoints,Termination,TerminationRequester,ID)
        end
    end.

% Function called when there is no message to read, and process do not need to terminate.
noMessage([X,Y],EntryPoints,ExitPoints,Termination,TerminationRequester,ID) ->
    mainRobot([X,Y],EntryPoints,ExitPoints,Termination,TerminationRequester,ID).
