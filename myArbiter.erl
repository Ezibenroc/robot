-module(myArbiter).
-import(arbiter,[startArbiter/4]).
-export([handleAction/4]).

handleAction(Pid, Params, State, Debug) ->
    case Params of
        % Delay of 250ms for a move
        [move,[X1,Y1],[X2,Y2]] ->
            Start = myLists:get_(X1,Y1,State),
            End = myLists:get_(X2,Y2,State),
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1)
                    or (Start =/= "r")
                    ->  timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,invalid,[X1,Y1],[X2,Y2]]}), State;
                End =/= "o" 
                    -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,blocked,[X1,Y1],[X2,Y2]]}), State;
                true -> timer:send_after(250,self(),{arbiterRequest,Pid,action,[handlemove,ok,[X1,Y1],[X2,Y2]]}), myLists:set_(X2,Y2,'x',myLists:set_(X1,Y1,'x',State))
            end;
        [handlemove,Action,[X1,Y1],[X2,Y2]] -> 
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, myLists:set_(X2,Y2,'r',myLists:set_(X1,Y1,'o',State))
            end;
        _ -> io:fwrite("HandleActions: received unknown Params.\n"), State
    end.
