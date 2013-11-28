%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: ETL.erl
%%% @author Niklas Larsson
%%% @doc
%%%	The mother module for the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(etl).
-export([start/0, stop/0, reload/0, relaunch/0, loop/1, force/0]).
-include("../include/ETL.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% start/0 - Starts the ETL and makes the links.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start() -> {ok, pid()}).
start() ->
	case whereis(?ETL) of
		undefined ->
			register(?ETL, spawn(fun init/0)),
			start();
		PID ->
			{ok, PID}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/0 - Initilaization of the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> any()).
init() ->
	process_flag(trap_exit, true),
	inets:start(),
	ModuleList = launch(?ETL_CONFIG),
	loop(ModuleList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% stop/0 - Stops the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stop() -> stopped | already_stopped).
stop() ->
	case whereis(?ETL) of
		undefined ->
			already_stopped;
		_ ->
			?ETL ! {action, stop},
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% reload/0 - Reloads the codebase in the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(reload() -> ok).
reload() ->
	?ETL ! {action, reload},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% relaunch/0 - 	Relaunches the modules the etl supervises. Should only be
%%%					used after reload/0 has been invoked.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(relaunch() -> ok).
relaunch() ->
	?ETL ! {action, relaunch},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% force/0 - Forces the functions inside the scheduler config to extecute.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(force() -> ok).
force() ->
	?ETL ! {action, 'force extract'},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% loop/1 - 	Loops the ETL. The argument is a list of tuples with two elements
%%%				where the first is a pid and the second is the name of that pid.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(loop(list()) -> any()).
loop(ModuleList) ->
	receive
		{action, stop} ->
			send_to_all(ModuleList, {action, stop}),
			ok;

		{action, reload} ->
			send_to_all(ModuleList, {action, reload}),
			etl:loop(ModuleList);

		{action, relaunch} ->
			NewModuleList = launch(?ETL_CONFIG),
			loop(NewModuleList);

		{action, 'force extract'} ->
			{ok, Config} = scheduler:get_config(),
			forceExtract(Config),
			loop(ModuleList);

		{'EXIT', FromPid, _Reason} ->
			%log?
			NewModuleList = revive(ModuleList, FromPid),
			loop(NewModuleList)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% forceExtract/1 - Starts every fun in a config from the scheduler
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(forceExtract(list()) -> ok).
forceExtract([]) ->
	ok;

forceExtract([{_, Fun, _, _} | Tail]) ->
	Fun(),
	forceExtract(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% module_factory/1 - Starts the given module and create a link.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(module_factory(atom()) -> record()).
module_factory(Module) ->
	{ok, Pid} = Module:start(),
	link(Pid),
	#moduleinfo{module=Module, pid=Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% launch/1 - Starts all the modules in a list.s
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(launch([atom(), ...]) -> [record(), ...]).
launch([]) ->
	[];
launch([Head | Tail]) ->
	[module_factory(Head) | launch(Tail)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% send_to_all/2 - Sends the message to all the modules.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(send_to_all([record(), ...], any()) -> ok).
send_to_all([], _) ->
	ok;
send_to_all([Head | Tail], Message) ->
	Head#moduleinfo.pid ! Message,
	send_to_all(Tail, Message).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% revive/2 - Find the module that belongs to the pid and restarts it.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(revive([record(), ...], pid()) -> [record(), ...]).
revive([], _) ->
	[];
revive([Head | Tail], Pid) when Head#moduleinfo.pid == Pid ->
	[module_factory(Head#moduleinfo.module) | Tail];
revive([Head | Tail], Pid) ->
	[Head | revive(Tail, Pid)].