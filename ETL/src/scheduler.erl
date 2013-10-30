%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: scheduler.erl
%%%	@author Niklas Larsson
%%% @doc
%%%	
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(scheduler).
-export([start/0, stop/0, update/1, remove/1, get_config/0, get_config/1]).
-include("../include/ETL.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% start/0 - Starts the scheduler.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start() -> {ok, pid()}).
start() ->
	case whereis(?SCHEDULER) of
		undefined ->
			register(?SCHEDULER, spawn(fun init/0)),
			start();
		PID ->
			{ok, PID}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% stop/0 - Stops the scheduler.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stop() -> stopped | already_stopped).
stop() ->
	case whereis(?SCHEDULER) of
		undefined ->
			already_stopped;
		_ ->
			?SCHEDULER ! {action, stop},
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/0 - Initilaization of the scheduler.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> ok).
init() ->
	DefaultConfig = ?LAUNCH_LIST,
	loop(DefaultConfig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% loop/1 - The loops that handles everything.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(loop(list()) -> ok).
loop(ConfigList) ->
	receive
		{action, stop} ->
			ok;
		{update, Config} ->
			NewConfigList = updateConfig(Config, ConfigList),
			loop(NewConfigList);
		{remove, ConfigTag} ->
			NewConfigList = removeConfig(ConfigTag, ConfigList),
			loop(NewConfigList);
		{getConfig, Pid} ->
			Pid ! {config, ConfigList},
			loop(ConfigList);
		{getConfig, Pid, default} ->
			Pid ! {config, ?LAUNCH_LIST},
			loop(ConfigList)
	after 
		?UPDATE_INTERVAL ->
			check(ConfigList),
			loop(ConfigList)
	end,
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% check/1 - Takes a config list and sends the second element to the receiver.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(check([{atom(), fun(), [{pos_integer()} | {pos_integer(), pos_integer()} | {pos_integer(), pos_integer(), pos_integer()}, ...], atom()}, ...]) -> ok).
check([]) -> ok;

check([{_, _, [], _} | T]) ->
	check(T);

check([{Tag, Fun, [Time | TimeTail], Receiver} | Tail]) ->
	{H, M, S} = time(),
	case tuple_size(Time) of
		1 ->
			if 
				{H} == Time ->
					Receiver ! {start, Fun};
				true -> ok
			end;
		2 ->
			if
				{H, M} == Time ->
					Receiver ! {start, Fun};
				true -> ok
			end;
		3 ->
			if
				{H, M, S} == Time ->
					Receiver ! {start, Fun};
				true -> ok
			end
	end,
	check([{Tag, Fun, TimeTail, Receiver} | Tail]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% update/1 - 	Adds the configuration it the tag is not pressent or updates if
%%%				if the tag is already in the configuration
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(update([{atom(), fun(), [{pos_integer()} | {pos_integer(), pos_integer()} | {pos_integer(), pos_integer(), pos_integer()}, ...], atom()}, ...]) -> ok).
update(Config) ->
	?SCHEDULER ! {update, Config},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% remove/1 - 	Deletes the config with that tag.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(remove(atom()) -> ok).
remove(ConfigTag) ->
	?SCHEDULER ! {remove, ConfigTag},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% get_config/0 - Gets the configuration
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(get_config(default) -> {ok, list()}).
get_config(default) ->
	?SCHEDULER ! {getConfig, self(), default},
	receive
		{config, Config} ->
			{ok, Config}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% get_config/0 - Gets the configuration
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(get_config() -> {ok, list()}).
get_config() ->
	?SCHEDULER ! {getConfig, self()},
	receive
		{config, Config} ->
			{ok, Config}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% updateConfig/2 - 	Updates the tupple with the specified tag, if the tag 
%%%						isn't in the list then add the config to the end.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(updateConfig(list(), list()) -> list()).
updateConfig(Config, []) -> [Config | []];

updateConfig({Tag, Fun, List, Receiver}, [ {Tag, _, _, _} | Tail]) ->
	[{Tag, Fun, List, Receiver} | Tail];

updateConfig(Config, [Head | Tail]) ->
	[Head | updateConfig(Config, Tail)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% removeConfig/2 - 	Removes the tuple with the specific tag.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(removeConfig(atom(), list()) -> list()).
removeConfig(_, []) -> [];

removeConfig(Tag, [{Tag, _, _, _} | Tail]) ->
	Tail;

removeConfig(Tag, [Head | Tail]) ->
	[Head | removeConfig(Tag, Tail)].