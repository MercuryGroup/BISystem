% Mercry group transform stock template.

-module(nyse_t).
-export([start/0, stop/0, init/0, transform/1, sendData/1, loop/1]).

start() ->
	register(nyse_t,spawn(?MODULE,loop,[self()])).

stop() ->
	nyse_t ! {stop}.

init() ->
	ok.

transform(Stock) ->
	ok.

sendData([]) ->
	ok.

loop(From) ->
	receive
		{stop}-> stopped;
		RawStock -> transform(RawStock),
		loop(From)

	end.