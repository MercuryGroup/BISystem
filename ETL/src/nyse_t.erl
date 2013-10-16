% Mercry group transform stock template.

-module(nyse_t).
-export([start/0, stop/0, init/0, transform/1, sendData/1, loop/1,priceConvert/1]).

start() ->
	register(nyse_t,spawn(?MODULE,loop,[self()])).

stop() ->
	nyse_t ! {stop}.

init() ->
ok.

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
		 % io:format("~p~n",[StockChange]).


volumeConvert(Vol) -> case string:sub_string(Vol,string:len(Vol)) of
	("M") -> NewVol = re:replace(Vol,"M","0000000",[{return,list}]),re:replace(NewVol,"\\.","",[{return,list}]);
	("K") -> NewVol = re:replace(Vol,"K","000",[{return,list}]),re:replace(NewVol,"\\.","",[{return,list}]);
	_-> Vol
end.

priceConvert(Price) ->
[First,Second] =re:split(Price,"\\.",[{return,list}]),
{Firstint,_} = string:to_integer(First),
{Secondint,_} = string:to_integer(Second),
TransPrice = io_lib:format("~.2f",[(Firstint+(0.01*Secondint))*0.7]),
StringPrice = lists:nth(1,TransPrice),
case string:sub_string(Price,1,1) of
	("+")-> string:concat("+",StringPrice);
	("-")->string:concat("-",StringPrice);
	_ -> StringPrice
end.


	




sendData([]) ->
	ok.

loop(From) ->
	receive
		{stop}-> stopped;
		RawStock ->transform(RawStock), 	
		loop(From)

	end.