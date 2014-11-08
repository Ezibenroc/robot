-module(robotUtils).
-export([allPids/0,allNames/0,killRobots/0]).

allNames() ->
    IsRobot =
        fun (Name) ->
            case lists:sublist(atom_to_list(Name),6) of
                "Robot-" -> {true,Name};
                _ -> false
            end
        end,
    lists:zf(IsRobot,registered()).

allPids() -> lists:map(fun (X) -> whereis(X) end, allNames()).

killRobots() -> lists:map(fun (X) -> exit(X,kill) end, allPids()).

% Send Mess to all processes of the list.
multiSend([],_) -> ok;
multiSend([H|T],Mess) -> H ! Mess, multiSend(T,Mess).
