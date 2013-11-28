%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: newsrss_e.erl
%%% @author Robin Larsson <Robin Larsson@TM5741>
%%% @doc
%%% Server extract module for extracting stock market news.
%%% Currently supporting Yahoo Finance News RSS Feed, but can be developed
%%% to be generic. And is adapted to only retrieve the news for
%%% the current retrival date.
%%%
%%% Supports XML element filtering, and parsing of date & time strings.
%%% All the retrieved data is sent to the Load part (DB) of the ETL.
%%% NOTE: In order to retrieve news for stock market indexes there needs to be
%%% a "^" character added before the symbol name, needed in order to follow
%%% the Yahoo Finance News RSS Feed API.
%%% @end
%%% Created : 11 Oct 2013 by <Robin Larsson@TM5741>
%%% Modified: 28 Nov 2013 by <Robin Larsson@TM5741>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(newsrss_e).
-author("Robin Larsson <Robin Larsson@TM5741>").
% sendData are deprecated for exporting since 2013-11-11, but exists for
% future changes
-compile({nowarn_unused_function, [{sendData, 1}]}).
-export([start/0, stop/0, init/0, loop/0, getData/1]).
% http://stackoverflow.com/a/18846096
-include_lib("../include/ETL.hrl").
% https://github.com/erlang/otp/blob/maint/lib/xmerl/include/xmerl.hrl
-include_lib("../include/R16B01/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Starting the extracting server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start() -> {ok, pid()}).
start() ->
	case whereis(?NEWS) of
		undefined ->
			Pid = erlang:spawn(?MODULE, init, []),
			erlang:register(?NEWS, Pid),
			{ok, Pid};
		Process ->
			{ok, Process}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Stops the extracting server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stop() -> stopped | already_stopped).
stop() ->
	case whereis(?NEWS) of
		undefined ->
			already_stopped;
		_ ->
			?NEWS ! {action, stop},
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends a request to the server to retrieve news items (concurrently)
%%% for each provided stock/market symbol.
%%% Symbols = string() e.g. "yhoo,aapl,^ftse,3in.l,aak.st".
%%%
%%% Example call setup:
%%% newsrss_e:getData("yhoo,aapl,^ftse,3in.l,aak.st").
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(getData(Symbols :: string()) -> ok).
getData(Symbols) ->
	?NEWS ! {self(), symbol, Symbols}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends the data retrieved by the server to the Load layer (DB).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(sendData(Data :: term()) -> ok).
sendData(Data) ->
	%io:format("~p~n", [Data]). % For testing purposes
	?LOAD ! Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Initialises the main server process for news extracting.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> ok).
init() ->
	loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Message receive loop for the extract server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(loop() -> ok).
loop() ->
	receive
		{action, reload} ->
			?NEWS:loop();
		{action, stop} ->
			ok;
		{_From, symbol, Symbols} ->
			% ***
			% See getData/1 methods for more code info.
			% Adapted for supporting multiple retrivals for
			% news symbols, but unused ATM and remaining for
			% future changes.
			% ***
			% Used for returning results from spawned processes
			Pid = self(),
			% Extracting each symbol into a separate string, stored in a list
			SymbolsPost = string:tokens(Symbols, ","),
			% For retrieving and parsing the XML data
			RetrieveParseXML = fun(Symbol) ->
				spawn(fun() ->
					Parsed = element(1,
						parseXML(
						retrieveXML(
						"http://feeds.finance.yahoo.com/rss/2.0/headline?s=" ++
						Symbol ++ "&region=US&lang=en-US"))),
					% For testing purposes
					% Parsed = element(1, 
					% 	parseXML(
					% 	retrieveXML(
					% 		"http://tankmaster.no-ip.org/test/error.xml"))),
					% Configuration settings for the news feed retrival
					XMLSearchInfo = [{symbolMarket, Symbol},
					{childItem, item},
					{filterItems, [title, link, description, pubDate]},
					{databaseID, guid},
					{dateTimeField, pubDate},
					{dateTimeSort, currentDay},
					{marketMapping, [{"l", "lse"}, {"st", "omx"}]}],
					Pid ! {self(), processXML(Parsed, XMLSearchInfo)}
				end)
			end,
			% Executing the parallel map that retrieves data for each
			% symbol (each a spawned process).
			Processes = lists:map(RetrieveParseXML, SymbolsPost),
			% Reading the results from the spawned processes
			% Enabling synchronous behaviour.
			% Using retrieveResult/1 for sending away the result
			% from the spawned processes
			retrieveResult(Processes),
			loop()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Reads the messages from each of the processes, and combining
%%% into a list result.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(retrieveResult([pid(), ...] | []) -> ok).
retrieveResult([]) ->
	ok;
retrieveResult([Process | Rest]) ->
	receive
		{Process, Result} ->
			case Result of
				[] -> % No result was returned from the retrival process
					ok;
				_Result ->
					sendData(Result),
					retrieveResult(Rest)
			end
	after 10000 -> % Aborting after 10 secs,
				   % if the spawned process e.g. crashes
		ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Parses the XML data, and returning a record based format.
%%% The record based format is defined in the xmerl library.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(parseXML(XMLData :: string()) -> tuple()).
parseXML(XMLData) ->
	xmerl_scan:string(XMLData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Retrieves data from the specified URL, through the HTTP protocol.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(retrieveXML(Link :: string() | _) ->
	string() | {error, atom() | term()}).
retrieveXML(Link) when is_list(Link) ->
	% Used for setting up the sockets needed for the HTTP client
	inets:start(),
	% Asynchroneous request
	% http://www.erlang.org/doc/man/httpc.html under receiver
	% "Defaults to the pid() of the process calling
	% the request function (self())."
	case httpc:request(get, {Link, []}, [], [{sync, false}]) of
		{ok, _Val} ->
			receive
				{http, {_RequestId, Result}} ->
					{_HTTPStatus, _HTTPHeader, XMLData} = Result,
					checkCallExceeding(binary:bin_to_list(XMLData))
			after 30000 ->
				{error, no_connection_or_no_data_returned}
			end;
		{error, Error} ->
			{error, Error}
	end;
retrieveXML(_) -> % Not a vaild link
	{error, non_valid_link}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% *Explicitly for responses from Yahoo Finance API*
%%% Seeing whether the API call limit has been exceeded.
%%% If not return the supplied data from the argument.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(checkCallExceeding(DataString :: string()) ->
	string() | {error, atom()}).
checkCallExceeding(DataString) ->
	% The parsing is explicitly looking at the XML structure
	% defined here http://developer.yahoo.com/search/rate.html
	ParsedXML = element(1, xmerl_scan:string(DataString)),
	case ParsedXML#xmlElement.expanded_name of
		'Error' ->
			MainContent = lists:nth(2, ParsedXML#xmlElement.content),
			[PossibleError] = MainContent#xmlElement.content,
			case PossibleError#xmlText.value of
				"limit exceeded" -> % A limit message has been received
					throw({error, yahoo_rss_call_limit_exceeded});
				_ ->
					DataString
			end;
		rss ->
			DataString
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Processes a XML document and returns data based on the provided
%%% XMLSearchInfo.
%%% Returns an Erlang term (mostly a tuple) with the same hierarchy structure
%%% as the provided XML document.
%%% 
%%% XMLSearchInfo shall contain search info; XML names for XML elements that
%%% shall be processed.
%%% 
%%% XMLSearchInfo = [{symbolMarket, list()},
%%%					{childItem, atom()},
%%%					{filterItems, [atom(), ...]},
%%%					{databaseID, guid},
%%%					{dateTimeField, atom()},
%%%					{dateTimeSort, atom()},
%%%					{marketMapping, {{atom(), atom()}, {atom(), atom()}}].
%%%
%%% ParsedXML = tuple(). Parsed with xmerl_scan:string(string())
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(processXML(ParsedXML :: tuple(), XMLSearchInfo :: list()) -> list()).
processXML(ParsedXML, XMLSearchInfo) ->
	% Extracting the search info
	{_, SymbolMarket} = lists:keyfind(symbolMarket, 1, XMLSearchInfo),
	{_, LookForChildElement} = lists:keyfind(childItem, 1, XMLSearchInfo),
	{_, FilterElements} = lists:keyfind(filterItems, 1, XMLSearchInfo),
	{_, DatabaseID} = lists:keyfind(databaseID, 1, XMLSearchInfo),
	{_, DateTimeFieldName} = lists:keyfind(dateTimeField, 1, XMLSearchInfo),
	{_, DateTimeSort} = lists:keyfind(dateTimeSort, 1, XMLSearchInfo),
	{_, MarketMapping} = lists:keyfind(marketMapping, 1, XMLSearchInfo),
	% Contains all the child elements
	[MainContent] = ParsedXML#xmlElement.content,
	ItemListContent = MainContent#xmlElement.content,
	% Using a list comprehension, which goes through each found
	% XML element node.
	XMLPreResult = 
		[extractChildElementsList(SymbolMarket, MarketMapping, El,
			FilterElements, DatabaseID)
		|| El <- extractMainElementsList(
			ItemListContent, LookForChildElement)],
	% Adaptation for Yahoo Finance News Feed, for current day retrival of news
	XMLPostResult =
		[processMainElements(El, [], DateTimeFieldName, DateTimeSort)
		|| El <- XMLPreResult],
	[{news, El} || El <- XMLPostResult, El =/= {null, []}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML main elements (those that are located under
%%%	the root XML node, and as well contains child elements).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractMainElementsList(MainElementsList :: [tuple(), ...],
	LookForChildElement :: atom()) -> list()).
extractMainElementsList(MainElementsList, LookForChildElement) ->
	[El#xmlElement.content || El <- MainElementsList,
	erlang:element(1, El) == xmlElement,
	% The child element names to look for
	El#xmlElement.name == LookForChildElement].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML child elements in the XML main elements.
%%% Support for filtering, converting and adding XML elements.
%%% Adapted to Yahoo Finance News Feed API.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractChildElementsList(SymbolMarket :: list(),
	MarketMapping :: [{atom(), atom()}, ...],
	ChildElementsList:: [tuple(), ...], FilterElements :: [atom(), ...],
	DatabaseID :: atom()) -> list()).
extractChildElementsList(SymbolMarket, MarketMapping, ChildElementsList,
	FilterElements, DatabaseID) ->
	% Checking whether some elements shall be filtered away or converted,
	% as well even added (latter specific for Yahoo Finance News Feed).
	% ATM a date & time string is the only element that can be converted.
	% For adding there are only news item, symbol and market ID.
	% The elements that are filtered are noted as {null, []}, and deleted
	% when found.
	ExtractedChildElements = lists:delete({null, []},
		[processChildElements(El, FilterElements, DatabaseID)
		|| El <- ChildElementsList]), % Removing potentially skipped elements
	% Adding 'type' for distinction in DB
	PreResultType = lists:append(ExtractedChildElements, [{type, "news"}]),
	% Adding 'symbol' and 'market' for distinction in DB 
	[Symbol, Market] =
		case string:tokens(SymbolMarket, ".") of
			[_Symbol, _Market] ->
				TrueMarketName = proplists:get_value(_Market, MarketMapping),
				[_Symbol, TrueMarketName];
			[_Symbol] -> % Exception case, no market == nyse
				[_Symbol, "nyse"]
		end,
	PreResultSymbol = lists:append(PreResultType, [{symbol, Symbol}]),
	lists:append(PreResultSymbol, [{market, Market}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Processes XML main elements with post-processing
%%% (list elements not applicable as Xmerl record types); filter, convert.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type(element() :: [{atom(), list()}, ...]).
-spec(processMainElements(
	element() | [], Acc :: [tuple(), ...] | [],
	DateTimeFieldName :: atom(), DateTimeSort :: atom()) ->
	{null, []} | element()).
processMainElements([], [], _, _) ->
	{null, []};
processMainElements([], Acc, _, _) ->
	Acc;
processMainElements([{Name, Value} | T], Acc,
	DateTimeFieldName, DateTimeSort) ->
	case [Name, DateTimeSort] of
		[DateTimeFieldName, currentDay] -> % Match found for current day news.
			[Day, LetterMonth, Year, _, _] =
				string:tokens(lists:last(string:tokens(Value, ",")), " "),
			NumberMonth = monthNumber(LetterMonth),
			CurrentDate = erlang:date(),
			DateToTest = {erlang:list_to_integer(Year),
				NumberMonth, erlang:list_to_integer(Day)},
			case DateToTest of
				CurrentDate -> % Match found, converting date & time
							   % to a timestamp and caching.
					processMainElements(T, 
						lists:append(Acc, [{Name,
							erlang:integer_to_list(
								timestampConverter(Value))}]),
						DateTimeFieldName, DateTimeSort);
				_ -> % Match not found at all, aborting the search.
					processMainElements([], [],
						DateTimeFieldName, DateTimeSort)
			end;
		_ -> % Match not found, adding to cache and continuing the search.
			processMainElements(T, lists:append(Acc, [{Name, Value}]),
				DateTimeFieldName, DateTimeSort)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Processes XML child elements; filter, convert and add.
%%% Converting and adding is specifically for Yahoo Finance News Feed.
%%% Otherwise made to be generic.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(processChildElements(Element :: tuple(), FilterElements :: [atom(), ...],
	DatabaseID :: atom()) -> tuple() | list()).
processChildElements(Element, FilterElements, DatabaseID) ->
	case Element#xmlElement.name of
		% For adding the unique news item ID, specific for
		% Yahoo Finance News Feed
		DatabaseID ->
			% Removing unecessary characters from the result.
			{_, Result} = lists:split(length("yahoo_finance/"),
				extract_XMLText(Element#xmlElement.content)),
			% ToExtract = extract_XMLText(Element#xmlElement.content),
			% % "Ugly" test for seeing if the symbol is not available
			% % at the Yahoo Finance News Feed API.
			% {_, Result} = case ToExtract of
			% 	[] ->
			% 		{error, no_valid_symbol};
			% 	_ToExtract ->
			% 		% Removing unecessary characters from the result.
			% 		lists:split(length("yahoo_finance/"),
			% 			_ToExtract)
			% end,
			{'_id', Result};
		Other ->
			% Filtering out the XML elements that shall not be included
			case lists:member(Other, FilterElements) of
				true -> % Not to be filtered
					{Element#xmlElement.name,
					findingSplitUpChildElements(Element#xmlElement.content)};
				false -> % To be filtered
					{null, []}
			end				
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Handling if there are XML elements that has been split up by
%%% quote signs, that Xmerl sees as the end of the XML value.
%%% And as well extracting the XML elements name, and their values.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(findingSplitUpChildElements(
	MultipleChildElements :: [tuple(), ...]) -> list()).
findingSplitUpChildElements(MultipleChildElements) ->
	% Concatenates the value(s) of the elements that has been split up.
	lists:concat(
		% Extracting the value of each element.
		lists:map(
			fun(El) ->
				extract_XMLText([El])
			end,
			MultipleChildElements
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Extracts the value from a XML element.
%%% If there is no value in it, a blank list is returned.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type(xmlText() :: #xmlText{}).
-spec(extract_XMLText(xmlText() | []) -> string()).
extract_XMLText([]) -> % If there is no value for the XML element
	"";
extract_XMLText(Content) ->
	% Taking the head of the list,
	% used to find the value of a XML element
	Item = erlang:hd(Content),
	% Checking whether the XML element, is a
	% record type of a XML element that has text as a value
	case element(1, Item) of
		xmlText -> % If found
			Item#xmlText.value;
		_ -> % If not found
			""
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts a date & time string (explicitly with time zone GMT/UTC)
%%% into a timestamp string. The timestamp will be in milliseconds, and follow
%%% the Epoch time standard. The date & time string shall the RFC822 format
%%% standard in section 5 of its specification.
%%% Example of a date & time string "Wed, 22 Oct 2013 15:53:37 GMT".
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(timestampConverter(DateTimeString :: string()) -> integer()).
timestampConverter(DateTimeString) ->
	% lists:last returns everything, except the weekday from the
	% date & time string.
	% string:tokens returns tokens based on a delimiter.
	[Day, Month, Year, Time, _] =
		string:tokens(lists:last(string:tokens(DateTimeString, ",")), " "),
	[Hour, Minutes, Seconds] = string:tokens(Time, ":"),
	% First converts the date & time to gregorian seconds
	GregSeconds = calendar:datetime_to_gregorian_seconds(
		{{erlang:list_to_integer(Year), monthNumber(Month),
		erlang:list_to_integer(Day)},
		{erlang:list_to_integer(Hour), erlang:list_to_integer(Minutes),
		erlang:list_to_integer(Seconds)}}),
	% Seconds convert from gregorian seconds to Epoch time in milliseconds
	% http://www.epochconverter.com ,
	% under "How to get the current epoch time in ..."
	% http://stackoverflow.com/questions/8805832/number-of-seconds-from-
	% 1st-january-1900-to-start-of-unix-epoch
	(GregSeconds - (719528*24*3600)) * 1000. %% *1000 for milliseconds

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts the textual description of a month (three characters) to a number
%%% representation.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(monthNumber(MonthString :: string()) -> pos_integer()).
monthNumber(MonthString) when length(MonthString) == 3  ->
	case MonthString of
		"Jan" -> 1;
		"Feb" -> 2;
		"Mar" -> 3;
		"Apr" -> 4;
		"May" -> 5;
		"Jun" -> 6;
		"Jul" -> 7;
		"Aug" -> 8;
		"Sep" -> 9;
		"Oct" -> 10;
		"Nov" -> 11;
		"Dec" -> 12
	end.