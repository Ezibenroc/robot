-module(myRobot).
-export([multiSend/2,mainRobot/3]).
-include_lib("eunit/include/eunit.hrl").

-define(TIME_REC, 50).

% Memory format: [X,Y],Termination,TerminationRequester
%    [X,Y] is the position

% When one robot receive {terminate_request}, he handle all its last messages, then send an ack and quit.

% Send Mess to all processes of the list.
multiSend([],_) -> ok;
multiSend([H|T],Mess) -> H ! Mess, multiSend(T,Mess).

% Function called to start a robot.
mainRobot([X,Y],Termination,TerminationRequester) ->
    receive
        {PID,terminate_request} -> mainRobot([X,Y],true,[PID|TerminationRequester])
    after ?TIME_REC ->
        if
            Termination -> multiSend(TerminationRequester,{self(),ackTerminate}), terminated;
            true -> noMessage([X,Y],Termination,TerminationRequester)
        end
    end.

% Function called when there is no message to read, and process do not need to terminate.
noMessage([X,Y],Termination,TerminationRequester) ->
    mainRobot([X,Y],Termination,TerminationRequester).
