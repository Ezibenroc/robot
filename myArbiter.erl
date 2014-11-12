-module(myArbiter).
-import(arbiter,[startArbiter/4]).
-export([handleAction/4,start/0]).
-include_lib("eunit/include/eunit.hrl").
%-define(TIME_MOVE, 250).
%-define(TIME_ENTER, 1000).
-define(TIME_MOVE, 5).
-define(TIME_ENTER, 10).


% HandleAction function for the arbiter.
% Handled actions:
%   [move,[X1,Y1],[X2,Y2]]
%   [handlemove,Status,[X1,Y1],[X2,Y2]]
handleAction(Pid, Params, State, _) ->
    {Entry,Exit,Map} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite("HandleActions: received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        % MOVE
        % Delay of 250ms for a move
        [move,{X1,Y1},{X2,Y2}] ->
            {Start,End} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {"invalid","invalid"};
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1)
                    or (Start =/= "r")
                    ->  timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,invalid,{X1,Y1},{X2,Y2}]}), State;
                End =/= "o" 
                    -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,blocked,{X1,Y1},{X2,Y2}]}), State;
                true -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,ok,{X1,Y1},{X2,Y2}]}), {Entry,Exit,myLists:set_(X2,Y2,"x",myLists:set_(X1,Y1,"x",Map))}
            end;
        % HANDLEMOVE
        [handlemove,Action,{X1,Y1},{X2,Y2}] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {Entry,Exit,myLists:set_(X2,Y2,"r",myLists:set_(X1,Y1,"o",Map))}
            end;
        % ENTER
        [enter,EntryPoint,RobotName] ->
        {Xentry,Yentry,EntryState} = try lists:nth(EntryPoint,Entry) of
                {X,Y} -> {X,Y,myLists:get_(X,Y,Map)};
                _ -> error
            catch
                error:function_clause -> {"invalid","invalid","invalid"};
                X -> X
            end,
        if
            (EntryState =:= "invalid")
                ->  timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,invalid,{Xentry,Yentry},RobotName]}), State;
            (EntryState =/= "o")
                -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,blocked,{Xentry,Yentry},RobotName]}), State;
            true -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,ok,{Xentry,Yentry},RobotName]}), {Entry,Exit,myLists:set_(Xentry,Yentry,"x",Map)}
        end;
        % HANDLEENTER
        [handleenter,Action,{Xentry,Yentry},_] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {Entry,Exit,myLists:set_(Xentry,Yentry,"r",Map)}
            end;
        % MISC
        Err2 -> io:fwrite("HandleActions: received unknown Params: ~w.\n",[Err2]), State
    end.

handleInfo(PID,Params,State,_) ->
    {Entry,_,_} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite("HandleActions: received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        [debug] -> PID ! State, State;
        [entry] -> PID ! {entries,Entry}
    end.

start() ->
    startArbiter(fun handleAction/4, fun handleInfo/4, myLists:getState(), true).
