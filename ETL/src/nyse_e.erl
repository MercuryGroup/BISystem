%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: extractnyse.erl
%%% @author Magnus Hernegren
%%% @doc
%%% Extracts stock information from NYSE via
%%% http://money.cnn.com/data/markets/nyse/
%%% @end
%%% Created 11 October 2013 (Friday),10:02 by Magnus Hernegren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(nyse_e).
-export([start/0, init/0, getData/1, sendData/1, loop/1]).
-include ("../include/ETL.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Start one process for each of the pages containing stocks.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() -> init().


-spec init() -> any().
init() ->   inets:start(), PidList = [spawn(fun() -> pageSelector(N) end) || N <- lists:seq(1,128)],   {ok, PidList} .
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Takes an integer which decides which page to load. Cuts off the uneccessary 
%%% data from the loaded page and splits the remaining data into a list of 
%%% strings where each string
%%% is an individual stock.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pageSelector(N) ->  
PageNumber = lists:flatten(io_lib:format("~p", [N])),
{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
      httpc:request(string:concat
      	("http://money.cnn.com/data/markets/nyse/?page=",PageNumber)), 
      {_,[[{Start,_}],[{Stop,_}]]} = re:run(Body,"tbody",[global]),
      StockData = string:substr(Body, Start+7, (Stop-Start)-8), 
      StockList = re:split(StockData,"<tr>",[{return,list}]),
      {Pno,_}=string:to_integer(PageNumber),
      loop({StockList,2,Pno}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Loops through the list of stocks from each page.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec loop(State :: any()) -> any().
loop({StockList,Number,PageNumber}) ->

case Number=<length(StockList) of
	true  -> ListSegment = lists:nth(Number,StockList),
	sendData(getData(findData(ListSegment,ListSegment,1,1,[]))),
	loop({StockList,Number+1,PageNumber});
	_ -> case PageNumber=<128 of
		% true -> pageSelector(PageNumber+1);
		_-> io:format("finished page ~p~n",[PageNumber])
	end
end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Responsible for finding the data between the tags, stops when </tr> is 
%%% encountered.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findData(SubList,List,N,M,Datalist) ->
	case SubList of
			("<") -> Test = string:substr(List,N,M+4), case Test of
				("</tr>") -> Datalist;
				_->findData(string:substr(List,N+1,M),List,N+1,M,Datalist)
			end;
			(">") -> Test = string:substr(List,N,M+1), case Test of
				("><") -> findData(string:substr(List,N+1,M),List,N+1,M,Datalist);
				_-> Dlist = lists:append(Datalist,
					[findWord(string:substr(List,N+1,M),List,N+1,M)]), 
				findData(string:substr(List,N+1,M),List,N+1,M,Dlist)
			end;
			_ ->findData(string:substr(List,N+1,M),List,N+1,M,Datalist)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% If a word is encountered in findData, this function finds its length and 
%%% returns it.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findWord(SubList,List,N,M) -> 
case string:chr(SubList,$<) of
	(0) -> findWord(string:substr(List,N,M+1),List,N,M+1);
	_-> string:substr(List,N,M-1)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Arranges the data into a more structured format
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec getData(State :: any()) -> {NewState :: any(), Stock :: [{atom(), any()}, ...]}.
getData(State) ->
[Symbol,Name,Price,Change,Percent,_,Volume,_] = 
State,
{stock,[{symbol,Symbol},{name, string:sub_string(Name,7)},{change, Change},
{latest, Price}, {percent, Percent},{volume, volumeConvert(Volume)},{market,"NYSE"},
{updated,?TIMESTAMP},{openVal,openingValCal(Price,Change)},{type,"stock"}]}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Sends the data to the transform module
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sendData([{atom(), any()}, ...]) -> ok.
sendData(SingleStockList) ->
	% io:format("~p",[SingleStockList]),
	?LOAD ! {SingleStockList}.

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
openingValCal(Price,Change) ->
{PriceFloat,_} = string:to_float(Price),
{ChangeFloat,_} = string:to_float(Change),
TransPrice = io_lib:format("~.2f",[(PriceFloat-ChangeFloat)*0.7]),
lists:nth(1,TransPrice).


