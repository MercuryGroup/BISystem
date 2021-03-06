%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: omx_c.erl
%%% @author Joel Berghé
%%% @doc
%%% Extractor module for the OMX stock exchange.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(omx_e).
-export([start/0, loop/3]).
-include("../include/ETL.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Performs initializing actions, such as starting web handler and fetching
%%% all raw data from the web page.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	%%Start inets
	inets:start(),
	%%Get data from web page
	{ok, {{_Version, _, _ReasonPhrase}, _Headers, _Body}} = httpc:request("http://www.netfonds.is/quotes/peers.php?paper=OMXSPI&exchange=ST"),
	%%Strip stock data of unnecessary code	
	{Body,_} = getString(_Body, "<th>Value</th>\n</tr>\n<tr>\n", "\n</div>\n"),
	%%Convert the string into a list
	RawList = re:split(Body, "\n<td class=\"left\">" ,[{return,list},group]),
	%%Get number of stocks in list
	_StockLength = length(RawList),
	%%Split list into four parts in order to distrubute work 
	%%Split to two lists
	{_TempList1, _TempList2} = lists:split(length(RawList) div 2, RawList),
	%%Split first temporary list
	{List1, List2} = lists:split(length(_TempList1) div 2, _TempList1),
	%%Split second temporary list
	{List3, List4} = lists:split(length(_TempList2) div 2, _TempList2),
	%%Create regex filter used for exracting each information from the web page
	ParseFilters = 
		[{name, "<a title=\"", "\" href=\""},
		{symbol, "\">", "</a>"},
		{latest, "T\">", "</td><td class"},
		{change, "\">", "</td>"},
		{percent, "\">", "</td>"},
		{closingVal, "<td>", "</td>"},
		{openVal, "<td>", "</td>"},
		{volume, "\">", "</td>"}],
	%%Spawn processes which will send individual parts of the stock data.
	spawn(omx_e, loop, [List1, ParseFilters, ?LOAD]),
	spawn(omx_e, loop, [List2, ParseFilters, ?LOAD]),
	spawn(omx_e, loop, [List3, ParseFilters, ?LOAD]),
	spawn(omx_e, loop, [List4, ParseFilters, ?LOAD]),
	%%Send market data to database
	sendData(getMarketData(), ?LOAD).

%
%	%%TEMP
%	temp(0, StockLength-1).
%temp(M, M) -> 
%	receive
%		Pid -> io:format("~p: ~p~n", [M+1, Pid])
%	end;
%temp(N, M) ->
%	receive
%		Pid -> io:format("~p: ~p~n", [N+1, Pid]),
%		temp(N+1, M)
%		
%	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Iterate through all items in the given list of stocks, parse each item.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop([], _, _) ->
	ok;
loop([[String]|List], ParseFilters, Pid) ->
	%%Parse each stock
	Stock = getData(String, ParseFilters),
	sendData({stock, Stock}, Pid),
	loop(List, ParseFilters, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Parses the string containing stock data and converts it to a list of tuples.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Empty/end of list
getData(_, []) ->
	[{updated, ?TIMESTAMP}, {market, "OMX"}, {type, "Stock"}];
%%Fix opening value (and change to euro)
getData(Stock, [{openVal, FilterStart, FilterStop}|Filters]) ->
	{_OpenVal, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	%%If the opening value doesn't show on the web page, the value within  the
	%%the source code will be "&nbsp". This will then be replaced with "-".
	case _OpenVal of
		"&nbsp;" ->
			OpenVal = "-";
		_ ->
			OpenVal = currencyWrapper(re:replace(_OpenVal, "\\s+", "", [global,{return,list}]))
	end,
	[{openVal, OpenVal}|getData(TrimmedStock, Filters)];
%%Latest (change value to euro)
getData(Stock, [{latest, FilterStart, FilterStop}|Filters]) ->
	{_Latest, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	case _Latest of
		"-" ->
			Latest = _Latest;
		_ ->
			Latest = currencyWrapper(re:replace(_Latest, "\\s+", "", [global,{return,list}]))
	end,
	[{latest, Latest}|getData(TrimmedStock, Filters)];
%%Change (change value to euro)
getData(Stock, [{change, FilterStart, FilterStop}|Filters]) ->
	{Change, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	[{change, currencyWrapper(Change)}|getData(TrimmedStock, Filters)];
%%Skip closing value
getData(Stock, [{closingVal, FilterStart, FilterStop}|Filters]) ->
	{_ClosingVal, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	getData(TrimmedStock, Filters);
%%Remove all blank spaces in volume value
getData(Stock, [{volume, FilterStart, FilterStop}|Filters]) ->
	{_Volume, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	Volume = re:replace(_Volume, "\\s+", "", [global,{return,list}]),
	[{volume, Volume}|getData(TrimmedStock, Filters)];
%%All other stock values
getData(Stock, [{ValueName, FilterStart, FilterStop}|Filters]) ->
	{Value, TrimmedStock} = getString(Stock, FilterStart, FilterStop),
	[{ValueName, Value}|getData(TrimmedStock, Filters)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Get data about the OMX stock exchange.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
getMarketData() ->
	{ok, {{_, 200, _}, _, Csv}} = httpc:request("http://download.finance.yahoo.com/d/quotes.csv?s=^OMXSPI&f=l1c1p2opgh"),
	TrimmedCsv = re:replace(Csv, "(\"|\r|\n)", "", [global,{return,list}]),
	SplitCsv = re:split(TrimmedCsv, "[,]",[{return,list}]),
	MarketTuples = [{latest}, {change}, {percent}, 
	{openVal}, {closingVal}, {lowest}, {highest}],
	{market, parseMarketData(SplitCsv, MarketTuples)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Combine two lists, one containing stock exchange data and the other 
%%% containing tuples, and combine them into a single list.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%End of tuples list, add an extra tuple containing market name.
parseMarketData(_, []) -> [{updated, ?TIMESTAMP} | [{market, "OMX"}]];
parseMarketData([Value|Csv], [{percent}|Rest]) ->
	[{percent, Value}|parseMarketData(Csv, Rest)];
%%Iterate through the list and extract convert the CSV file into a list.
parseMarketData([Value|Csv], [{CurrentTuple}|Rest]) ->
	[{CurrentTuple, currencyWrapper(Value)}|parseMarketData(Csv, Rest)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Get substring within two given regex points. 
%%% Return both substring and rest of trimmed string.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
getString(String, Start, Stop) ->
	%%Get first part of text to be removed
	{_,[{Pos1, Length}]} = re:run(String, Start),
	%%Remove first part
	Trimmed = string:sub_string(String, Pos1+Length),
	%%Get length of useful code
	{_,[{Pos2,_}]} = re:run(Trimmed, Stop),
	%%Save code to new variable
	NewString = string:sub_string(Trimmed, 2 , Pos2),
	%%Get the rest of the code
	Rest = string:sub_string(Trimmed, Pos2),
	{NewString, Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	Send data to the specified Pid.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
sendData(Data, Pid) ->
	Pid ! Data.

currencyWrapper(N) -> currency ! {request, self(), {N, "SEK"}},
	receive
		{reply, R} -> R
	end.
