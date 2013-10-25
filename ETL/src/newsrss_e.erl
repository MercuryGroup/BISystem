%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: newsrss_e.erl
%%% @author Robin Larsson <Robin Larsson@TM5741>
%%% @doc
%%% Extract server module for extracting stock market news.
%%% Currently supporting Yahoo Finance News RSS Feed, but can be developed
%%% to be generic.
%%% Using datetime module from Lars Kiesow
%%% (https://github.com/lkiesow/erlang-datetime/)
%%% @end
%%% Created : 11 Oct 2013 by <Robin Larsson@TM5741>
%%% Modified: 24 Oct 2013 by <Robin Larsson@TM5741>
%%% TODO: 
%%% Implementing OTP patterns and behaviour.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(newsrss_e).
-author("Robin Larsson <Robin Larsson@TM5741>").
-export([start/0, stop/0, init/0, loop/1, getData/1, sendData/0]).
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
	case whereis(newsrss_e) of
		undefined ->
			Pid = spawn(newsrss_e, init, []),
			register(newsrss_e, Pid),
			{ok, Pid};
		Process ->
			{ok, Process}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% *Add stopping inet process if not used by any other process*
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stop() -> stopped | already_stopped).
stop() ->
	case whereis(newsrss_e) of
		undefined ->
			already_stopped;
		_ ->
			newsrss_e ! stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Used for receiving a message from a process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(getReply() -> term()).
getReply() ->
	receive
		{From, Command, Any} ->
			Any;
		Msg ->
			Msg
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Initialises the main server process for news extracting.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> stopped | term()).
init() ->
	loop([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Message receive loop for the extract server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(loop(list()) -> stopped | term()).
loop(Data) ->
	receive 
		{From, startGet, {Link, XMLSearchInfo}} ->
			Parsed = element(1, 
				parseXML(
				retrieveXML(Link))),
			Processed = processXML(Parsed, XMLSearchInfo),
			From ! ok,
			loop(Processed);
		{From, startSend} ->
			From ! Data;
		stopped ->
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Returns an Erlang term (with the appropiate data type) for each news feed.
%%% The returned result depends on the Options variable.
%%% Multiple terms can be returned by internal recursive calls.
%%% The Options variable shall follow this format:
%%% {{NewsFeedLink1,ExtractionOptions},{NewsFeedLink1,ExtractionOptions}, ...}.
%%% NewsFeedLinkN = string(),
%%% ExtractionOptions = tuple()
%%%
%%% newsrss_e:getData({"http://feeds.finance.yahoo.com/rss/2.0/headline?s=^ftse&region=US&lang=en-US", [{childItem, item}, {filterItems, [title, link, description, pubDate]}, {dateTimeField, pubDate}]}).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -spec(getData(Options :: ) -> ). Define correct type specifications.
getData(Options) ->
	newsrss_e ! {self(), startGet, Options},
	getReply().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData() ->
	newsrss_e ! {self(), startSend},
	getReply().

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
%%% XMLSearchInfo = [{childItem, atom()}, {filterItems, [atom(), ...]}, {dateTimeField, atom()}]. To be decided!
%%%
%%% ParsedXML = tuple(). Parsed with xmerl_scan:string(string())
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(processXML(tuple(), list()) -> list()).
processXML(ParsedXML, XMLSearchInfo) ->
	% Extracting the search info
	{_, LookForChildElement} = lists:keyfind(childItem, 1, XMLSearchInfo),
	{_, FilterElements} = lists:keyfind(filterItems, 1, XMLSearchInfo),
	{_, DateTimeFieldName} = lists:keyfind(dateTimeField, 1, XMLSearchInfo),
	% Contains all the child elements
	[MainContent] = ParsedXML#xmlElement.content,
	ItemListContent = MainContent#xmlElement.content,
	% Using a list comprehension, which goes through each found
	% XML element node.
	[{news, extractChildElementsList(El, FilterElements, DateTimeFieldName)}
	|| El <- extractMainElementsList(ItemListContent, LookForChildElement)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML main elements.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractMainElementsList([tuple(), ...], atom()) -> list()).
extractMainElementsList(MainElementsList, LookForChildElement) ->
	[El#xmlElement.content || El <- MainElementsList,
	erlang:element(1, El) == xmlElement,
	% The element name to find
	El#xmlElement.name == LookForChildElement].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Handling if there are XML elements that has been split up by
%%% quote signs, that Xmerl sees as the end of the XML value.
%%% And was well, extracting the XML elements name, and their values.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(findingSplitUpChildElements([tuple(), ...]) -> list()).
findingSplitUpChildElements(MultipleChildElements) ->
	lists:concat(
		lists:map(
			fun(El) ->
				extract_XMLText([El])
			end,
			MultipleChildElements
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Finding the XML child elements in the XML main elements.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(extractChildElementsList([tuple(), ...], [atom(), ...], atom()) -> list()).
extractChildElementsList(ChildElementsList, FilterElements,
	DateTimeFieldName) ->
	lists:delete({null, []}, [filterAndConvertElements(El, FilterElements, DateTimeFieldName)
	|| El <- ChildElementsList]). % Removing potentially skipped elements

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Filters and converts XML elements.
%%% Converting is specifically for Yahoo Finance News Feed.
%%% Otherwise generic.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(filterAndConvertElements(tuple(), [atom(), ...], atom()) -> tuple() | list()).
filterAndConvertElements(Element, FilterElements, DateTimeFieldName) ->
	case Element#xmlElement.name of
		% For converting date time strings to Epoch timestamps in milliseconds.
		% Though not done when included in FilterElements.
		DateTimeFieldName ->
			case lists:member(DateTimeFieldName, FilterElements) of
				true -> % Not to be filtered
					{DateTimeFieldName,
					integer_to_list(
						timestampConverter(
							findingSplitUpChildElements(
								Element#xmlElement.content
							)
						)
					)};
				false -> % To be filtered
					{null, []}
			end;
		Other ->
			% Filtering out the XML elements that shall not be included
			case lists:member(Other, FilterElements) of
				true ->
					{Element#xmlElement.name,
					findingSplitUpChildElements(Element#xmlElement.content)};
				false ->
					{null, []}
			end				
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts a date time string (explicitly with time zone GMT/UTC) into a
%%% into a timestamp. The timestamp will be in milliseconds, and follow
%%% the Epoch time standard. The date time string shall follow an English
%%% standard.
%%% Example of a date time string "Wed, 22 Oct 2013 15:53:37 GMT".
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(timestampConverter(string()) -> integer()).
timestampConverter(DateTimeString) ->
	% lists:last removes the weekday from the date time string.
	% string:tokens returns tokens based on a delimiter.
	[Day, Month, Year, Time, _] =
		string:tokens(lists:last(string:tokens(DateTimeString, ",")), " "),
	[Hour, Minutes, Seconds] = string:tokens(Time, ":"),
	% First converts the date time to gregorian seconds
	GregSeconds = calendar:datetime_to_gregorian_seconds(
		{{erlang:list_to_integer(Year), monthNumber(Month),
		erlang:list_to_integer(Day)},
		{erlang:list_to_integer(Hour), erlang:list_to_integer(Minutes),
		erlang:list_to_integer(Seconds)}}),
	% Seconds convert from gregorian seconds to Epoch time
	% http://www.epochconverter.com ,
	% under "How to get the current epoch time in ..."
	% http://stackoverflow.com/questions/8805832/number-of-seconds-from-
	% 1st-january-1900-to-start-of-unix-epoch
	GregSeconds - 719528*24*3600.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Extracts the value from a XML element.
%%% If there is no value in it, a blank Erlang list is returned.
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
	% Checking whether the XML element, is a Erlang
	% record type of a XML element that has text as a value
	case element(1, Item) of
		xmlText -> % If found
			Item#xmlText.value;
		_ -> % If not found
			""
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Parses the XML data, and returning a record based format.
%%% The record based format is defined in the xmerl library.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(parseXML(string()) -> tuple()).
parseXML(XMLData) ->
	ParsedXML = xmerl_scan:string(XMLData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Retrieves data from the specified URL, through the HTTP protocol.
%%% TODO: Correct error retrival handling needs to be implemented.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(retrieveXML(string()) -> string() | tuple()).
retrieveXML(Link) when is_list(Link) ->
	% Used for setting up the sockets needed for the HTTP client
	inets:start(),
	% Asynchroneous request
	% http://www.erlang.org/doc/man/httpc.html under receiver
	% "Defaults to the pid() of the process calling
	% the request function (self())."
	case httpc:request(get, {Link, []}, [], [{sync, false}]) of
		{ok, Val} ->
			receive
				{http, {RequestId, Result}} ->
					{HTTPStatus, HTTPHeader, XMLData} = Result,
					binary:bin_to_list(XMLData)
			after 30000 ->
				{error, no_connection_or_no_data_returned}
			end;
		{error, Error} ->
			{error, Error}
	end;
retrieveXML(_) -> % Not a vaild link
	{error, non_valid_link}.