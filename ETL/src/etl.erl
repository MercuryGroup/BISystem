%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: ETL.erl
%%% @author Niklas Larsson
%%% @doc
%%%	The mother module for the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(etl).
-export([start/0, stop/0, reload/0, loop/1, force/0]).
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
	%spawn the load
	{ok, PID} = load:start(),
	link(PID),
	%spawn the scheduler
	{ok, S_PID} = scheduler:start(),
	link(S_PID),
	%spawn the currency converter
	{ok, C_PID} = currency:start(),
	link(C_PID),

	List = [{PID, ?LOAD}, {S_PID, ?SCHEDULER}, {C_PID, ?CURRENCY}],
	loop(List).

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
loop(List) ->
	receive
		{start, Fun} ->
			Fun(),
			loop(List);

		{action, stop} ->
			load:stop(),
			scheduler:stop(),
			ok;

		{action, reload} ->
			?LOAD ! {action, reload},
			?CURRENCY ! {action, reload},
			etl:loop(List);

		{action, 'force extract'} ->
			{ok, Config} = scheduler:get_config(),
			forceExtract(Config),
			loop(List);

		{'EXIT', FromPid, _Reason} ->
			%log?
			%check who FromPid is and restart
			case whois(FromPid, List) of
				?LOAD ->
					{ok, Pid} = load:start(),
					link(Pid),
					NewList = replace(FromPid, Pid, List),
					loop(NewList);

				?SCHEDULER ->
					{ok, Pid} = scheduler:start(),
					link(Pid),
					NewList = replace(FromPid, Pid, List),
					loop(NewList);

				?CURRENCY ->
					{ok, Pid} = currency:start(),
					link(Pid),
					NewList = replace(FromPid, Pid, List),
					loop(NewList);

				undefined ->
					ok
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% whois/2 - Looks after the pid in the list of tuples
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(whois(pid(), list()) -> undefined | atom()).
whois(_, []) ->
	undefined;

whois(Pid, [{Pid, Name} | _]) ->
	Name;

whois(Pid, [_ | Tail]) ->
	whois(Pid, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% replace/3 - replaces the old pid with the new pid in the list.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(replace(OldPid :: pid(), NewPid :: pid(), list()) -> list()).
replace(_, _, []) ->
	[];

replace(OldPid, NewPid, [{OldPid, Name} | Tail]) ->
	[{NewPid, Name} | Tail];

replace(OldPid, NewPid, [Head | Tail]) ->
	[Head | replace(OldPid, NewPid, Tail)].

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