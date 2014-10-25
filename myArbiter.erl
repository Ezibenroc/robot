-module(myArbiter).
-import(arbiter,[startArbiter/4]).
-export([handleAction/4,start/0]).
-include_lib("eunit/include/eunit.hrl").

% HandleAction function for the arbiter.
% Handled actions:
%   [move,[X1,Y1],[X2,Y2]]
%   [handlemove,Status,[X1,Y1],[X2,Y2]] (problem of security?)
handleAction(Pid, Params, State, _) ->
    case Params of
        % MOVE
        % Delay of 250ms for a move
        [move,[X1,Y1],[X2,Y2]] ->
            Start = myLists:get_(X1,Y1,State),
            End = myLists:get_(X2,Y2,State),
            if
            % TODO : check that the coordinates are in the map.
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1)
                    or (Start =/= "r")
                    ->  timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,invalid,[X1,Y1],[X2,Y2]]}), State;
                End =/= "o" 
                    -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,blocked,[X1,Y1],[X2,Y2]]}), State;
                true -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,ok,[X1,Y1],[X2,Y2]]}), myLists:set_(X2,Y2,"x",myLists:set_(X1,Y1,"x",State))
            end;
        % HANDLEMOVE
        [handlemove,Action,[X1,Y1],[X2,Y2]] -> 
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, myLists:set_(X2,Y2,"r",myLists:set_(X1,Y1,"o",State))
            end;
        % MISC
        _ -> io:fwrite("HandleActions: received unknown Params.\n"), State
    end.

handleInfo(PID,_,State,_) ->
    PID ! State,
    State.

start() ->
    startArbiter(fun handleAction/4, fun handleInfo/4, myLists:getMap(), true).
