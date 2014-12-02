% Second arbiter module. Functions handleAction, handleInfo and start.

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


% Handling of action queries by the arbiter.
handleAction(Pid, Params, State, _) ->
    {Entry,Exit,Map} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite(standard_error,"Arbiter: HandleActions received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        % MOVE (delay of 250ms)
        [move,{X1,Y1},{X2,Y2}] ->
            % Try to collect the state of the cell (exception if out of the map).
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r") % invalid
                    ->  timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,invalid,{X1,Y1},{X2,Y2}]}), State;
                End =/= " " % blocked
                    -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,blocked,{X1,Y1},{X2,Y2}]}), State;
                true % possible move
                    -> timer:send_after(?TIME_MOVE,self(),{arbiterRequest,Pid,action,[handlemove,ok,{X1,Y1},{X2,Y2}]}), {Entry,Exit,myLists:set_(X2,Y2,{"o",GoldEnd},Map)}
            end;
        % HANDLING OF THE MOVE REQUEST
        [handlemove,Action,{X1,Y1},{X2,Y2}] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {{_,GoldStart},{_,GoldEnd}} = {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)},
                    {Entry,Exit,myLists:set_(X2,Y2,{"r",GoldEnd},myLists:set_(X1,Y1,{" ",GoldStart},Map))}
            end;
        % COLLECT (delay of 500ms)
        [collect,{X1,Y1},{X2,Y2},Student] ->
            % Try to collect the state of the cell (exception if out of the map).
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r") or (End =/= " ") or (GoldEnd =:= 0) % invalid
                    ->  timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,invalid,{X1,Y1},{X2,Y2},Student]}), State;
                true % there is gold to collect
                    -> io:fwrite(standard_error,"Arbiter:\tstudent ~w scored ~w\n",[Student,GoldEnd]),
                    { listener, ?UI_NODE } ! {someonescored,Student,GoldEnd}, % send the score to the UI
                    try superarbiter ! {score, Student, GoldEnd, self()} of % send the score to superarbiter
                        _ -> timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,ok,{X1,Y1},{X2,Y2},Student]}), {Entry,Exit,myLists:set_(X2,Y2,{End,0},Map)}
                    catch
                        error:badarg -> io:fwrite(standard_error,"Arbiter:\tfailed to send message to superarbiter.\n",[]),
                            timer:send_after(?TIME_COLLECT,self(),{arbiterRequest,Pid,action,[handlecollect,ok,{X1,Y1},{X2,Y2},Student]}), {Entry,Exit,myLists:set_(X2,Y2,{End,0},Map)};
                        SomeError -> SomeError
                    end
            end;
        % HANDLING OF THE COLLECT REQUEST
        [handlecollect,Action,{_,_},{_,_},_] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                ok -> Pid ! ok, State
            end;
        % ENTER (delay of 1000ms)
        [enter,EntryPoint,RobotName] ->
        {Xentry,Yentry,{EntryState,GoldEntry}} = try lists:nth(EntryPoint,Entry) of
                {X,Y} -> {X,Y,myLists:get_(X,Y,Map)};
                _ -> error
            catch
                error:function_clause -> {"invalid","invalid",{"invalid",0}};
                X -> X
            end,
        if
            (EntryState =:= "invalid") % invalid
                ->  timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,invalid,{Xentry,Yentry},RobotName]}), State;
            (EntryState =/= " ") % blocked
                -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,blocked,{Xentry,Yentry},RobotName]}), State;
            true % possible entry
                -> timer:send_after(?TIME_ENTER,self(),{arbiterRequest,Pid,action,[handleenter,ok,{Xentry,Yentry},RobotName]}), {Entry,Exit,myLists:set_(Xentry,Yentry,{"o",GoldEntry},Map)}
        end;
        % HANDLING OF THE ENTER REQUEST
        [handleenter,Action,{Xentry,Yentry},_] ->
            case Action of
                invalid -> Pid ! invalid, State ;
                blocked -> Pid ! blocked, State ;
                ok -> Pid ! ok, {_,GoldEntry} = myLists:get_(Xentry,Yentry,Map), {Entry,Exit,myLists:set_(Xentry,Yentry,{"r",GoldEntry},Map)}
            end;
        % UNKNOWN REQUEST
        _ -> io:fwrite(standard_error,"Arbiter:\tHandleAction received unknown Params: ~w.\n",[Params]), State
    end.

% Handling of info queries by the arbiter.
handleInfo(PID,Params,State,_) ->
    {Entry,Exit,Map} = case State of
        {Entry_,Exit_,Map_} -> {Entry_,Exit_,Map_};
        Err -> io:fwrite(standard_error,"Arbiter:\tHandleActions received wrong formated State: ~w.\n",[Err]), State
    end,
    case Params of
        % UI
        [ui,robots] -> io:fwrite(standard_error,"UI request:\t~w\n",[[ui,robots]]),{ listener, ?UI_NODE } ! {robotList,robotUtils:allNames()}, State;
        % DEBUG
        [debug] -> PID ! State, State;
        % ENTRY
        [entry] -> PID ! {entries,Entry};
        % ANALYZE
        [analyze,{X1,Y1},{X2,Y2}] ->
            % Try to collect the state of the cell (exception if out of the map).
            {{Start,_},{End,GoldEnd}} = try {myLists:get_(X1,Y1,Map),myLists:get_(X2,Y2,Map)} of
                    X -> X
                catch
                    error:function_clause ->  {{"r",0},{"x",0}}; % out of the map, so the robot is blocked
                    X -> X
                end,
            if
                (abs(X1-X2) > 1) or (abs(Y1-Y2) > 1) or (Start =/= "r") % invalid
                    ->  PID ! invalid ;
                End =:= "x" % blocked
                    -> PID ! {blocked,nomessage} ;
                End =:= "r" % robot
                    -> PID ! {robotname,nomessage} ;
                true -> 
                    if
                        GoldEnd > 0 -> PID ! {gold,nomessage} ; % there is gold
                        true ->
                            IsEntry = lists:any((fun(X) -> X=:={X2,Y2} end),Exit),
                            if
                                IsEntry -> PID ! {exit,nomessage}; % exit cell
                                true -> PID ! {empty,nomessage} % free cell
                            end
                    end
            end
    end.

start(State) ->
    startArbiter(fun handleAction/4, fun handleInfo/4, State, false).
