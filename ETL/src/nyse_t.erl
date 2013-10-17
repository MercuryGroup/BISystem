%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: nyse_t.erl
%%% @author Magnus Hernegren
%%% @doc
%%% 
%%% @end
%%% Created 15 October 2013 (Tuesday),  by Magnus Hernegren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nyse_t).
-export([start/0, stop/0, init/0, transform/1, sendData/1, loop/1,priceConvert/1]).

start() ->
	register(nyse_t,spawn(?MODULE,loop,[self()])).

stop() ->
	nyse_t ! {stop}.

init() ->
ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Transforms the data in the stock to appropriate values.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transform(Stock) ->
		{_,X} = lists:keyfind(name,1,Stock),
		StockName = lists:keystore(name,1,Stock,{name,string:sub_string(X,7)}),
		{_,Vol} = lists:keyfind(volume,1,Stock),
		StockVolume = lists:keystore(volume,1,StockName,{volume,volumeConvert(Vol)}),
		{_,Price} = lists:keyfind(latest,1,Stock),
		StockPrice = lists:keystore(price,1,StockVolume,{price,priceConvert(Price)}),
		{_,Change} = lists:keyfind(change,1,Stock),
		StockChange = lists:keystore(change,1,StockPrice,{change,priceConvert(Change)}),
		sendData(StockChange).
		% io:format("~p~n",[StockVolume]),


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts the volume from e.g. "1.6M" to "1600000".
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
volumeConvert(Vol) -> case string:sub_string(Vol,string:len(Vol)) of
	("M") -> NewVol = re:replace(Vol,"M","0000000",[{return,list}]),re:replace(NewVol,"\\.","",[{return,list}]);
	("K") -> NewVol = re:replace(Vol,"K","000",[{return,list}]),re:replace(NewVol,"\\.","",[{return,list}]);
	_-> Vol
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Converts the price/change in price to the appropriate currency
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
priceConvert(Price) ->
PriceDot = re:replace(Price, "\\,", "\\.", [{return, list}]), 
[First,Second] =re:split(PriceDot,"\\.",[{return,list}]),
{Firstint,_} = string:to_integer(First),
{Secondint,_} = string:to_integer(Second),
TransPrice = io_lib:format("~.2f",[(Firstint+(0.01*Secondint))*0.7]),
StringPrice = lists:nth(1,TransPrice),
case string:sub_string(Price,1,1) of
	("+")-> string:concat("+",StringPrice);
	("-")->string:concat("-",StringPrice);
	_ -> StringPrice
end.


	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends the transformed data to the loader
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData([]) ->
ok;


sendData(List) ->
List.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Recieves data from the extractor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(From) ->
	receive
		{stop}-> stopped;
		RawStock ->transform(RawStock), 	
		loop(From)

	end.