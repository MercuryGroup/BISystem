% Load part of the mercery group ETL project, TERM 3.
% Niklas Larsson & Rickard Bremer
% MERCURY GROUP
-module(loadstocks).
-export([start/0, stop/0, init/0, loop/1, sendData/1, convert/1]).

-spec start() -> {ok, pid()}.
start() ->
	ok.

stop() ->
	ok.

init() ->
	ok.

sendData(_String) ->
	ok.

convert([]) ->
	ok.

