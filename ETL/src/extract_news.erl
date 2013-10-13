%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: extract_news.erl
%%% @author Robin Larsson <Robin Larsson@TM5741>
%%% @doc
%%% Main module for extracting stock market news.
%%% To compile: c(extract_news, [{outdir,"../ebin/"}]).
%%% @end
%%% Created : 11 Oct 2013 by <Robin Larsson@TM5741>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(extract_news).
% https://github.com/erlang/otp/blob/maint/lib/xmerl/include/xmerl.hrl
-include_lib("../include/R16B01/xmerl.hrl").
-author("Robin Larsson <Robin Larsson@TM5741>").
-export([start/0, stop/0, init/0, loop/1]).
	%, retrieveData/1, getData/1, sendData/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() ->
	case whereis(extract_news) of
		undefined ->
			Pid = spawn(extract_news, init, []),
			register(extract_news, Pid),
			extract_news ! start,
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
	case whereis(extract_news) of
		undefined ->
			already_stopped;
		_ ->
			extract_news ! stopped,
			getReply()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Used for receiving a message from a process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec getReply() -> term().
getReply() ->
	receive
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
	loop([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec loop(term()) -> stopped | term().
loop(Temp) ->
	receive 
		start ->
			Parsed = element(1, parseXML(
				retrieveXML(
					{self(), "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D'http%3A%2F%2Ffinance.yahoo.com%2Fq%3Fs%3Dyhoo'%20and%20xpath%3D'%2F%2Fdiv%5B%40id%3D%22yfi_headlines%22%5D%2Fdiv%5B2%5D%2Ful%2Fli%2Fa'%20limit%205&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"}))),
			io:format("~p~n", [Parsed]),
			loop([]);
		stopped ->
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processXML() ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseXML(XMLData) ->
	xmerl_scan:string(XMLData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retrieveXML({Pid, Link}) when is_list(Link) ->
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
					Pid ! binary:bin_to_list(XMLData)
			after 10000 ->
				{error, no_connection_or_no_data_returned}
			end;
		{error, Error} ->
			{error, Error}
	end;
retrieveXML(_) -> % Not a vaild link
	{error, non_valid_link}.