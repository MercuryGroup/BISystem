%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: currency.erl
%%% @author Justin Inácio
%%% @doc
%%% Extracts currency rates from the following url:
%%% http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.xchange%20where%20pair%20in%20(%22USDEUR%22,%22GBPEUR%22,%20%22SEKEUR%22,%20%22JPYEUR%22)&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(currency).
-export([start/0, init/0, call/1, stop/0, loop/1, analyze_info/1, get_rates/0, convert/2, update_rates/0]).
-include_lib("xmerl/include/xmerl.hrl").

-include("../include/ETL.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Starts the process. Also sets a 24 hour timer to rescrape currency rates.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	Pid = whereis(?CURRENCY),
	if Pid == undefined ->
		register(?CURRENCY, spawn(?CURRENCY, init, [])),
		{ok, whereis(?CURRENCY)};
		true -> {ok, Pid}
	end.

init() ->
	timer:apply_interval(86400000, ?CURRENCY, update_rates, []),
	loop(get_rates()),
	reply(self(), ok).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Main function which queries the server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call(Msg) ->
	?CURRENCY ! {request, self(), Msg},
		receive 
			{reply, Reply} ->
				Reply
			end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Reply function used to reply back to the PID which sent a message.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reply(Pid, Reply) ->
	Pid ! {reply, Reply}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Stops the server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
	call(stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts the value (Val) from one of four currencies (Currency) to EUR. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert(Val, Currency) ->
	call({Val, Currency}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Server call which calls the get_rates() function. Used to update the server's
%%% stored currency rates.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_rates() ->
	call(update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% The server loop.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
			reply(Pid, lists:nth(1, io_lib:format("~.2f", [element(1, string:to_float(Val))]))),
			loop(Db);
		{request, Pid, update} ->
			reply(Pid, ok),
			loop(get_rates()); 
		{request, Pid, stop} ->
			reply(Pid, stopping);
		{request, Pid, _} ->
			reply(Pid, error),
			loop(Db);
		{action, reload} ->
			currency:loop(Db)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Main function which is used to get the needed currency rates from yahoo.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Helper function which is used for getting the necessary data from the query. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_info(WebData) ->		
	Parsed = element(1, xmerl_scan:string(WebData)),
	Data = xmerl_xpath:string("//Rate/text()", Parsed),
	[{_, [_, _, _, _], _, _, USDEUR, _}, {_, [_, _, _, _], _, _, GBPEUR, _}, 
	{_, [_, _, _, _], _, _, SEKEUR, _}, {_, [_, _, _, _], _, _, JPYEUR, _}] = Data,
	{USDEUR, GBPEUR, SEKEUR, JPYEUR}.

