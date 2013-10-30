-module(currency).
-export([start/0, init/0, call/1, stop/0, loop/1, analyze_info/1, get_rates/0, convert/2, update_rates/0]).
-include_lib("xmerl/include/xmerl.hrl").

start() ->
	Pid = whereis(currency),
	if Pid == undefined ->
		register(currency, spawn(currency, init, [])),
		{ok, whereis(currency)};
		true -> {ok, already_running}
	end.

init() ->
	inets:start(),
	timer:apply_interval(86400000, currency, update_rates, []),
	loop(get_rates()).


call(Msg) ->
	currency ! {request, self(), Msg},
		receive 
			{reply, Reply} ->
				Reply
			end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.

stop() ->
	call(stop).

convert(Val, Currency) ->
	call({Val, Currency}).

update_rates() ->
	call(update).

loop(Db) ->
	receive
		{request, Pid, {Val, "USD"}} ->
			Rate = element(1, string:to_float(element(1, Db))),
			reply(Pid, lists:nth(1, io_lib:format("~.2f",[(Rate * element(1, string:to_float(Val)))]))),
			loop(Db);
		{request, Pid, {Val, "GBP"}} ->
			Rate = element(1, string:to_float(element(2, Db))),
			reply(Pid, lists:nth(1, io_lib:format("~.2f",[(Rate * element(1, string:to_float(Val)))]))),
			loop(Db);
		{request, Pid, {Val, "SEK"}} ->
			Rate = element(1, string:to_float(element(3, Db))),
			reply(Pid, lists:nth(1, io_lib:format("~.2f",[(Rate * element(1, string:to_float(Val)))]))),
			loop(Db);
		{request, Pid, {Val, "JPY"}} ->
			Rate = element(1, string:to_float(element(4, Db))),
			reply(Pid, lists:nth(1, io_lib:format("~.2f",[(Rate * element(1, string:to_float(Val)))]))),
			loop(Db);
		{request, Pid, {Val, "GBX"}} ->
			Rate = element(1, string:to_float(element(2, Db))),
			Pounds = element(1, string:to_float(Val)) / 100,
			reply(Pid, lists:nth(1, io_lib:format("~.2f",[(Rate * Pounds)]))),
			loop(Db);
		{request, Pid, {Val, "EUR"}} ->
			%reply(Pid, derp),
			reply(Pid, lists:nth(1, io_lib:format("~.2f", [element(1, string:to_float(Val))]))),
			loop(Db);
		{request, Pid, update} ->
			reply(Pid, ok),
			loop(get_rates()); 
		{request, Pid, stop} ->
			inets:stop(),
			reply(Pid, stopped);
		{request, Pid, _} ->
			reply(Pid, error),
			loop(Db)
	end.



get_rates() ->		
	URL = "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.xchange%20where%20pair%20in%20(%22USDEUR%22,%22GBPEUR%22,%20%22SEKEUR%22,%20%22JPYEUR%22)&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys",
	{Result, Info} = httpc:request(URL),
	case Result of
		error -> 
			{Result, Info};
		ok ->
			{{_Protocol, _Code, _CodeStr}, _Attrs, WebData} = Info,
			analyze_info(WebData)
		end.



analyze_info(WebData) ->		
	Parsed = element(1, xmerl_scan:string(WebData)),
	Data = xmerl_xpath:string("//Rate/text()", Parsed),
	[{_, [_, _, _, _], _, _, USDEUR, _}, {_, [_, _, _, _], _, _, GBPEUR, _}, 
	{_, [_, _, _, _], _, _, SEKEUR, _}, {_, [_, _, _, _], _, _, JPYEUR, _}] = Data,
	{USDEUR, GBPEUR, SEKEUR, JPYEUR}.

