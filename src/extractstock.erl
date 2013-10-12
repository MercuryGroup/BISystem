% Mercury group extract stock template.

-module(extractstock).
-export([start/0, stop/0, init/0, getData/1, sendData/1, loop/1]).

-spec start() -> {ok, pid()}.
start() ->
	ok.

-spec stop() -> stopped | already_stopped.
stop() ->
	ok.

-spec init() -> any().
init() ->
	ok.

-spec getData(State :: any()) -> {NewState :: any(), Stock :: [{atom(), any()}, ...]}.
getData(_State) ->
	ok.

-spec sendData([{atom(), any()}, ...]) -> ok.
sendData([]) ->
	ok.
-spec loop(State :: any()) -> any().
loop(_State) ->
	ok.
