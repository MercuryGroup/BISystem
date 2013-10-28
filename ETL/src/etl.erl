%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: ETL.erl
%%% @author Niklas Larsson
%%% @doc
%%%	The mother module for the ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(etl).
-export([start/0, stop/0]).
-include("../include/ETL.hrl").

start() ->
	case whereis(?ETL) of
		undefined ->
			register(?ETL, spawn(fun init/0)),
			start();
		PID ->
			{ok, PID}
	end.

init() ->
	{ok, PID} = loadstock:start(),
	link(PID),
	%spawn the scheduler
	{ok, S_PID} = Scheduler:start(),
	loop().


stop() ->
	case whereis(?ETL) of
		undefined ->
			?ETL ! {action, stop},
			stopped;
		_ ->
			already_stopped
	end.
	

loop() ->
	receive
		{start, Fun} -> 
			Fun(),
			loop();

		{action, stop} ->
			loadstock:stop()
	end.