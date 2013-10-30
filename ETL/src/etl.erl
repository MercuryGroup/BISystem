%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: ETL.erl
%%% @author Niklas Larsson
%%% @doc
%%%	The mother module for the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(etl).
-export([start/0, stop/0, reload/0, loop/1]).
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
	%spawn the load
	{ok, PID} = load:start(),
	link(PID),
	%spawn the scheduler
	{ok, S_PID} = scheduler:start(),
	link(S_PID),

	List = [{PID, ?LOAD}, {S_PID, ?SCHEDULER}],
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
%%% loop/1 - 	Loops the ETL. The argument is a list of tuples with two elements
%%%				where the first is a pid and the second is the name of that pid.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
			etl:loop(List);

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

				undefined ->
					ok
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% whois/2 - Looks after the pid in the list of tuples
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
replace(_, _, []) ->
	[];

replace(OldPid, NewPid, [{OldPid, Name} | Tail]) ->
	[{NewPid, Name} | Tail];

replace(OldPid, NewPid, [Head | Tail]) ->
	[Head | replace(OldPid, NewPid, Tail)].