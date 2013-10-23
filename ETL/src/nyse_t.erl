%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: nyse_t.erl
%%% @author Magnus Hernegren
%%% @doc
%%% 
%%% @end
%%% Created 15 October 2013 (Tuesday),  by Magnus Hernegren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(nyse_t).
-export([start/0, stop/0, init/0, sendData/1, loop/1]).
-include("../include/ETL.hrl").
start() ->
	register(nyse_t,spawn(?MODULE,loop,[self()])).

stop() ->
	nyse_t ! {stop}.

init() ->
ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends the transformed data to the loader
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData([]) ->
ok;


sendData(List) ->
%io:format("~p~n",[List]),
%List.
?LOAD ! {stock, List}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Recieves data from the extractor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(From) ->
	receive
		{stop}-> stopped;
		% {stock,RawStock,Currency} ->transform(RawStock,Currency), 	
		{stock,RawStock,Currency} ->sendData(RawStock), 
		loop(From)

	end.