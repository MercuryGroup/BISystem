%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: newsrss_e.erl
%%% @author Robin Larsson <Robin Larsson@TM5741>
%%% @doc
%%% Extract server module for extracting stock market news.
%%% To compile: c(newsrss_e, [{outdir,"../ebin/"}]).
%%% @end
%%% Created : 11 Oct 2013 by <Robin Larsson@TM5741>
%%% TODO: 
%%% Extracting the relevant content. Solving why xmerl_scan:string/1 is
%%% splitting up Strings that contains quotes (and other characters that
%%% needs escape signs). SOLUTION: Consolidate them together!
%%% Implementing OTP patterns and behaviour.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(newsrss_e).
% https://github.com/erlang/otp/blob/maint/lib/xmerl/include/xmerl.hrl
-include_lib("../include/R16B01/xmerl.hrl").
-author("Robin Larsson <Robin Larsson@TM5741>").
-export([start/0, stop/0, init/0, loop/0, getData/1]).
	%, retrieveData/1, sendData/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Starting the extracting server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
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
-spec stop() -> stopped | already_stopped.
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
-spec getReply() -> term().
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
-spec init() -> stopped | term().
init() ->
	loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Message receive loop for the extract server.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec loop() -> stopped | term().
loop() ->
	receive 
		{From, startGet, {Link, XMLSearchInfo}} ->
			% Each extraction is done by a separate process
			spawn(fun() ->
				Parsed = element(1, 
					parseXML(
					retrieveXML(Link))),
				% Parsed =
				% 	parseXML(
				% 	retrieveXML(Link)),
				Processed = processXML(Parsed, XMLSearchInfo),
				From ! {self(), result, Processed}
			end),
			loop();
		%{From, result, Result} ->
		%	loop(Result);
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
%%% newsrss_e:getData({"http://feeds.finance.yahoo.com/rss/2.0/headline?s=YHOO&region=US&lang=en-US",[{childItem, item}]}).
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -spec getData(Options :: ) -> . Define correct type specifications.
getData(Options) ->
	newsrss_e ! {self(), startGet, Options},
	getReply().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%sendData(Data) ->
%	ok.

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
%%% XMLSearchInfo = [{childItem, atom()}, ...]. To be decided!
%%% 
%%% ParsedXML = tuple(). Parsed with xmerl_scan:string(string())
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec processXML(tuple(), list()) -> list().
processXML(ParsedXML, XMLSearchInfo) ->
	% Extracting the search info
	{_, LookForChildElement} = lists:keyfind(childItem, 1, XMLSearchInfo),
	% Contains all the child elements
	[MainContent] = ParsedXML#xmlElement.content,
	ItemListContent = MainContent#xmlElement.content,
	% Using a list comprehension, which goes through each found
	% XML element node.
	% Finding the XML main elements.
	ExtractedMainElementsList =
		fun(MainElementsList) ->
			[El#xmlElement.content || El <- MainElementsList,
			erlang:element(1, El) == xmlElement,
			El#xmlElement.name == LookForChildElement] % The element name to find
		end,
	% Finding the XML child elements in the XML main elements.
	% Extracting the XML element name, and its value.
	ExtractedChildElementsList =
		fun(ChildElementsList) ->
			[{El#xmlElement.name, extract_XMLText(El#xmlElement.content)}
			|| El <- ChildElementsList]
		end,
	[ExtractedChildElementsList(El)
	|| El <- ExtractedMainElementsList(ItemListContent)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Extracts the value from a XML element.
%%% If there is no value in it, a blank Erlang list is returned.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type xmlText() :: #xmlText{}.
-spec extract_XMLText(xmlText() | []) -> string().
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
-spec parseXML(string()) -> tuple().
parseXML(XMLData) ->
	% *OLD* http://erlang.2086793.n4.nabble.com/Handling-UTF-8-data-when-
	% parsing-XML-using-xmerl-tp2117428p2117430.html
	%BinaryXMLData = unicode:characters_to_binary(XMLData);
	%ProcessedXMLData = binary:bin_to_list(BinaryXMLData);
	%ParsedXML = xmerl_scan:string(ProcessedXMLData).
	ParsedXML = xmerl_scan:string(XMLData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Retrieves data from the specified URL, through the HTTP protocol.
%%% TODO: Correct error retrival handling needs to be implemented.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec retrieveXML(string()) -> string() | tuple().
retrieveXML(Link) when is_list(Link) ->
	% Used for setting up the sockets needed for the HTTP client
	inets:start(),
	%io:format("~p~n", [Link]),
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
			after 10000 ->
				{error, no_connection_or_no_data_returned}
			end;
		{error, Error} ->
			{error, Error}
	end;
retrieveXML(_) -> % Not a vaild link
	{error, non_valid_link}.