%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: newsrss_e.erl
%%% @author Robin Larsson <Robin Larsson@TM5741>
%%% @doc
%%% Server extract module for extracting stock market news.
%%% Currently supporting Yahoo Finance News RSS Feed, but can be developed
%%% to be generic.
%%% Supports XML element filtering, and parsing of date & time strings.
%%% All the retrieved data is sent to the Load (DB).
%%% @end
%%% Created : 11 Oct 2013 by <Robin Larsson@TM5741>
%%% Modified: 11 Nov 2013 by <Robin Larsson@TM5741>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(newsrss_e).
-author("Robin Larsson <Robin Larsson@TM5741>").
% getData and sendData are deprecated since 2013-11-11, but exists for
% future changes
-compile({nowarn_unused_function, [{getData, 1}, {sendData, 1}]}).
-export([start/0, stop/0]).
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
			Pid = spawn(fun init/0),
			register(?NEWS, Pid),
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
			?NEWS ! stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends a request to the server to retrieve news items (concurrently)
%%% for each provided stock/market symbol.
%%% The returned result depends on the Options variable.
%%% The Options variable shall follow this format:
%%% {Symbols,ExtractionOptions}.
%%% Symbols = string() e.g. "yhoo,aapl,^ftse",
%%% ExtractionOptions = list() [{childItem, item},
%%%								{filterItems,
%%%								[title, link, description, pubDate]},
%%%								{databaseID, guid},
%%% 							{dateTimeField, pubDate}]
%%%
%%% Example call setup:
%%% newsrss_e:getData({"yhoo,aapl,^ftse", [{childItem, item},
%%% {filterItems, [title, link, description, pubDate]}, {databaseID, guid},
%%% {dateTimeField, pubDate}]}).
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(getData({string(), [tuple(), ...]}) -> ok).
getData(Options) ->
	?NEWS ! {self(), startGet, Options}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends the data retrieved by the server to the Load layer (DB).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(sendData(term()) -> ok).
sendData(Data) ->
	%io:format("~p~n", [Data]). % For testing purposes
	?LOAD ! Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Initialises the main server process for news extracting.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> stopped | term()).
init() ->
	loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Message receive loop for the extract server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(loop() -> stopped).
loop() ->
	receive
		{_From, startGet, {SymbolsPre, XMLSearchInfo}} ->
			% ***
			% See getData/1 methods for more code info.
			% Adapted for supporting multiple retrivals for
			% news symbols, but unused ATM and remaining for
			% future changes.
			% ***
			% Used for returning results from spawned processes
			Pid = self(),
			% Extracting each symbol into a separate string, stored in a list
			SymbolsPost = string:tokens(SymbolsPre, ","),
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
					Pid ! {self(), processXML(Parsed, XMLSearchInfo)}
				end)
			end,
			% Executing the parallel map that retrieves data for each
			% symbol (each a spawned process).
			Processes = lists:map(RetrieveParseXML, SymbolsPost),
			% Reading the results from the spawned processes
			% Enabling synchronous behaviour.
			% Using lists:append to merge all the lists returned by the
			% spawned processes
			Result = lists:append(retrieveResult(Processes)),
			% Sending away the result
			prepareToSend(Result),
			loop();
		stopped ->
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Reads the messages from each of the processes, and combining
%%% into a list result.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retrieveResult([]) ->
	[];
retrieveResult([Process | Rest]) ->
	receive
		{Process, Result} ->
			[Result | retrieveResult(Rest)]
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Reads through a list and calling a function to send each element away.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(prepareToSend(list()) -> ok).
prepareToSend([Last | []]) ->
	sendData(Last);
prepareToSend([H | T]) ->
	sendData(H),
	prepareToSend(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Parses the XML data, and returning a record based format.
%%% The record based format is defined in the xmerl library.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(parseXML(string()) -> tuple()).
parseXML(XMLData) ->
	xmerl_scan:string(XMLData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Retrieves data from the specified URL, through the HTTP protocol.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(retrieveXML(string() | _) -> string() | {error, atom() | term()}).
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
%%% Seeing whether the call limit has been exceeded.
%%% If not return the supplied data from the argument.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(checkCallExceeding(string()) -> string() | {error, atom()}).
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
%%% XMLSearchInfo = [{childItem, atom()},
%%%					{filterItems, [atom(), ...]},
%%%					{databaseID, guid},
%%%					{dateTimeField, atom()}].
%%%
%%% ParsedXML = tuple(). Parsed with xmerl_scan:string(string())
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(processXML(tuple(), list()) -> list()).
processXML(ParsedXML, XMLSearchInfo) ->
	% Extracting the search info
	{_, LookForChildElement} = lists:keyfind(childItem, 1, XMLSearchInfo),
	{_, FilterElements} = lists:keyfind(filterItems, 1, XMLSearchInfo),
	{_, DatabaseID} = lists:keyfind(databaseID, 1, XMLSearchInfo),
	{_, DateTimeFieldName} = lists:keyfind(dateTimeField, 1, XMLSearchInfo),
	% Contains all the child elements
	[MainContent] = ParsedXML#xmlElement.content,
	ItemListContent = MainContent#xmlElement.content,
	% Using a list comprehension, which goes through each found
	% XML element node.
	[{news, extractChildElementsList(El, FilterElements, DatabaseID,
		DateTimeFieldName)}
	|| El <- extractMainElementsList(ItemListContent, LookForChildElement)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML main elements (those that are located under
%%%	the root XML node, and as well containing child elements).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractMainElementsList([tuple(), ...], atom()) -> list()).
extractMainElementsList(MainElementsList, LookForChildElement) ->
	[El#xmlElement.content || El <- MainElementsList,
	erlang:element(1, El) == xmlElement,
	% The child element names to look for
	El#xmlElement.name == LookForChildElement].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML child elements in the XML main elements.
%%% Support for filtering, converting and adding XML elements.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractChildElementsList([tuple(), ...], [atom(), ...],
	atom(), atom()) -> list()).
extractChildElementsList(ChildElementsList, FilterElements, DatabaseID,
	DateTimeFieldName) ->
	% Checking whether some elements shall be filtered away or converted,
	% as well even added (latter specific for Yahoo Finance News Feed).
	% ATM a date & time string is the only element that can be converted.
	% For adding the only element is a news item ID.
	% The elements that are filtered are noted as {null, []}, and deleted
	% when found.
	ExtractedChildElements = lists:delete({null, []},
		[processElements(El, FilterElements, DatabaseID, DateTimeFieldName)
		|| El <- ChildElementsList]), % Removing potentially skipped elements
	% Added 'type' for distinction in DB
	lists:append(ExtractedChildElements, [{type, "news"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Processing XML elements; filter, convert and add.
%%% Converting and adding is specifically for Yahoo Finance News Feed.
%%% Otherwise made to be generic.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(processElements(tuple(), [atom(), ...],
	atom(), atom()) -> tuple() | list()).
processElements(Element, FilterElements, DatabaseID, DateTimeFieldName) ->
	case Element#xmlElement.name of
		% For adding the unique news item ID, specific for
		% Yahoo Finance News Feed
		DatabaseID ->
			% Removing unecessary characters from the result.
			{_, Result} = lists:split(
				length("yahoo_finance/"),
				extract_XMLText(Element#xmlElement.content)),
			{'_id', Result};
		% *OPTIONAL*
		% Converting date & time strings to Epoch timestamps, in milliseconds.
		% Though not done when included in FilterElements.
		DateTimeFieldName ->
			case lists:member(DateTimeFieldName, FilterElements) of
				true -> % Not to be filtered
					Timestamp = integer_to_list(
						timestampConverter(
								findingSplitUpChildElements(
									Element#xmlElement.content
								)
							)
						),
					{DateTimeFieldName, Timestamp};
				false -> % To be filtered
					{null, []}
			end;
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
-spec(findingSplitUpChildElements([tuple(), ...]) -> list()).
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
%%% the Epoch time standard. The date & time string shall follow an English
%%% standard.
%%% Example of a date & time string "Wed, 22 Oct 2013 15:53:37 GMT".
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(timestampConverter(string()) -> integer()).
timestampConverter(DateTimeString) ->
	% lists:last removes the weekday from the date & time string.
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
-spec(monthNumber(string()) -> pos_integer()).
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