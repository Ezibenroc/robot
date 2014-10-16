-module(factory).
-export([spawnFactory/3,spawnLoop/3,test/2,killFactory/0]).

% Spawn a factory registered under the name 'factory', which will spawn robots
% executing the function Module:Name with parameters [DefaultParameters|X] where
% X is the name of the robot.
spawnFactory(Module, Name, DefaultParameters) ->
	case whereis(factory) of
		undefined -> register(factory,spawn(factory,spawnLoop,[Module,Name,DefaultParameters]));
		_ -> io:fwrite("<spawnFactory> Killing old factory...~n"), killFactory(), spawnFactory(Module,Name,DefaultParameters)
	end.

% Loop executed by the factory (see above).
spawnLoop(Module,Name,DefaultParameters) ->
	receive {spawn,Amount,Pid} ->
		Pid ! {spawned, spawnRobots(Amount,Module,Name,DefaultParameters)}
	end,
	spawnLoop(Module,Name,DefaultParameters).

% Spawn Amount robots (see above for more details).
spawnRobots(0,_,_,_) -> [];
spawnRobots(Amount,Module,Name,DefaultParameters) ->
	(UniqueName =
		case erlang:now() of {Ms,S,Mus} ->
			list_to_atom(lists:concat(["Robot-",integer_to_list(Ms),integer_to_list(S),integer_to_list(Mus)]))
		end),
	io:fwrite("<factory> Spawning ~s with default presets.~n",[UniqueName]),
	register(UniqueName, spawn(Module,Name,lists:concat([DefaultParameters,[UniqueName]]))),
	[UniqueName | spawnRobots(Amount-1,Module,Name,DefaultParameters)].

% Kill the factory.
killFactory() ->
	case whereis(factory) of
		undefined -> error;
		Pid -> exit(Pid,kill)
	end.

% WTF does it do ?
test(Atom,Name) ->
	io:fwrite("<~s> ~s~n",[Name,Atom]),
	receive impossiblemessage717 -> haha after 10000 -> test(Atom,Name) end.
