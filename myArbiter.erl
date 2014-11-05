-module(myArbiter).
-import(arbiter,[startArbiter/4]).
-export([handleAction/4,start/0]).
-include_lib("eunit/include/eunit.hrl").

% HandleAction function for the arbiter.
% Handled actions:
%   [move,[X1,Y1],[X2,Y2]]
%   [handlemove,Status,[X1,Y1],[X2,Y2]]
handleAction(Pid, Params, State, _) ->
    {Entry,Exit,Map} = case Params of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        _ -> io:fwrite("HandleActions: received wrong formated State.\n"), State
    end,
    case Params of
        % MOVE
        % Delay of 250ms for a move
        [move,[X1,Y1],[X2,Y2]] ->
            {Start,End} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {"invalid","invalid"};
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1)
                    or (Start =/= "r")
                    ->  timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,invalid,[X1,Y1],[X2,Y2]]}), State;
                End =/= "o" 
                    -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,blocked,[X1,Y1],[X2,Y2]]}), State;
                true -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,ok,[X1,Y1],[X2,Y2]]}), {Entry,Exit,myLists:set_(X2,Y2,"x",myLists:set_(X1,Y1,"x",Map))}
            end;
        % HANDLEMOVE
        [handlemove,Action,[X1,Y1],[X2,Y2]] -> 
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {Entry,Exit,myLists:set_(X2,Y2,"r",myLists:set_(X1,Y1,"o",Map))}
            end;
        % MISC
        _ -> io:fwrite("HandleActions: received unknown Params.\n"), State
    end.

handleInfo(PID,_,State,_) ->
    PID ! State,
    State.

start() ->
    startArbiter(fun handleAction/4, fun handleInfo/4, myLists:getState(), true).
