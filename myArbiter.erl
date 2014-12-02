-module(myArbiter).
-import(arbiter,[startArbiter/4]).
-export([handleAction/4,start/1]).
-include_lib("eunit/include/eunit.hrl").
-define(UI_NODE, 'alice@abc.def').
-define(TIME_MOVE, 250).
-define(TIME_COLLECT, 500).
-define(TIME_ENTER, 1000).
%-define(TIME_MOVE, 1).
%-define(TIME_COLLECT, 2).
%-define(TIME_ENTER, 4).


% HandleAction function for the arbiter.
% Handled actions:
%   [move,[X1,Y1],[X2,Y2]]
%   [handlemove,Status,[X1,Y1],[X2,Y2]]
handleAction(Pid, Params, State, _) ->
    {Entry,Exit,Map} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite(standard_error,"Arbiter: HandleActions received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        % MOVE
        % Delay of 250ms
        [move,{X1,Y1},{X2,Y2}] ->
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r")
                    ->  timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,invalid,{X1,Y1},{X2,Y2}]}), State;
                End =/= " "
                    -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,blocked,{X1,Y1},{X2,Y2}]}), State;
                true -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,ok,{X1,Y1},{X2,Y2}]}), {Entry,Exit,myLists:set_(X2,Y2,{"o",GoldEnd},Map)}
            end;
        % HANDLEMOVE
        [handlemove,Action,{X1,Y1},{X2,Y2}] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {{_,GoldStart},{_,GoldEnd}} = {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)},
                    {Entry,Exit,myLists:set_(X2,Y2,{"r",GoldEnd},myLists:set_(X1,Y1,{" ",GoldStart},Map))}
            end;
        % COLLECT
        % Delay of 500ms
        [collect,{X1,Y1},{X2,Y2},Student] ->
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r") or (End =/= " ") or (GoldEnd =:= 0)
                    ->  timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,invalid,{X1,Y1},{X2,Y2},Student]}), State;
                true -> io:fwrite(standard_error,"Arbiter:\tstudent ~w scored ~w\n",[Student,GoldEnd]),
                    { listener, ?UI_NODE } ! {someonescored,Student,GoldEnd},
                    try superarbiter ! {score, Student, GoldEnd, self()} of
                        _ -> timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,ok,{X1,Y1},{X2,Y2},Student]}), {Entry,Exit,myLists:set_(X2,Y2,{End,0},Map)}
                    catch
                        error:badarg -> io:fwrite(standard_error,"Arbiter:\tfailed to send message to superarbiter.\n",[]),
                            timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,ok,{X1,Y1},{X2,Y2},Student]}), {Entry,Exit,myLists:set_(X2,Y2,{End,0},Map)};
                        SomeError -> SomeError
                    end
            end;
        % HANDLECOLLECT
        [handlecollect,Action,{_,_},{_,_},_] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                ok -> Pid ! ok, State
            end;
        % ENTER
        % Delay of 1000ms
        [enter,EntryPoint,RobotName] ->
        {Xentry,Yentry,{EntryState,GoldEntry}} = try lists:nth(EntryPoint,Entry) of
                {X,Y} -> {X,Y,myLists:get_(X,Y,Map)};
                _ -> error
            catch
                error:function_clause -> {"invalid","invalid",{"invalid",0}};
                X -> X
            end,
        if
            (EntryState =:= "invalid")
                ->  timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,invalid,{Xentry,Yentry},RobotName]}), State;
            (EntryState =/= " ")
                -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,blocked,{Xentry,Yentry},RobotName]}), State;
            true -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,ok,{Xentry,Yentry},RobotName]}), {Entry,Exit,myLists:set_(Xentry,Yentry,{"o",GoldEntry},Map)}
        end;
        % HANDLEENTER
        [handleenter,Action,{Xentry,Yentry},_] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {_,GoldEntry} = myLists:get_(Xentry,Yentry,Map), {Entry,Exit,myLists:set_(Xentry,Yentry,{"r",GoldEntry},Map)}
            end;
        % MISC
        _ -> io:fwrite(standard_error,"Arbiter:\tHandleAction received unknown Params: ~w.\n",[Params]), State
    end.

handleInfo(PID,Params,State,_) ->
    {Entry,Exit,Map} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite(standard_error,"Arbiter:\tHandleActions received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        [ui,robots] -> io:fwrite(standard_error,"UI request:\t~w\n",[[ui,robots]]),{ listener, ?UI_NODE } ! {robotList,robotUtils:allNames()}, State;
        [debug] -> PID ! State, State;
        [entry] -> PID ! {entries,Entry};
        [analyze,{X1,Y1},{X2,Y2}] ->
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r")
                    ->  PID ! invalid ;
                End =:= "x"
                    -> PID ! {blocked,nomessage} ;
                End =:= "r"
                    -> PID ! {robotname,nomessage} ;
                true -> 
                    if
                        GoldEnd > 0 -> PID ! {gold,nomessage} ;
                        true ->
                            IsEntry = lists:any((fun(X) -> X=:={X2,Y2} end),Exit),
                            if
                                IsEntry -> PID ! {exit,nomessage};
                                true -> PID ! {empty,nomessage}
                            end
                    end
            end
    end.

start(State) ->
    startArbiter(fun handleAction/4, fun handleInfo/4, State, false).
