%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: extractnyse.erl
%%% @author Magnus Hernegren
%%% @doc
%%% 
%%% @end
%%% Created :  by Magnus Hernegren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(nyse_e).
-export([start/0, stop/0, init/0, getData/1, sendData/1, loop/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Start one process for each of the pages containing stocks.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() ->
% case whereis(nyseextract) == undefined of
   % true -> 
   [spawn(fun() -> pageSelector(N) end) || N <- lists:seq(1,128)], stopped.
%    register(nyseextract, spawn(extractnyse,init,[])),
%   {ok,whereis(nyseextract)};
%   _ -> {ok,whereis(nyseextract)}
% end.
 % PID = register(nyseextract, spawn(?MODULE, init,[])),{ok, PID}.
 % init().
	

-spec stop() -> stopped | already_stopped.
stop() -> ok.

-spec init() -> any().
init() ->  pageSelector(1).
	
-spec getData(State :: any()) -> {NewState :: any(), Stock :: [{atom(), any()}, ...]}.
getData(State) ->
[Symbol,Name,Price,_Change,Percent,_,Volume,_] = 
State,[{symbol,Symbol},{name, Name},{latest, Price}, {percent, Percent}, {volume, Volume}].
 % io:format("~p~n",[[{name, Name},{latest, Price}, {percent, Percent}, {volume, Volume}]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Takes an integer which decides which page to load. Cuts off the uneccessary data from the 
%%% loaded page and splits the remaining data into a list of strings where each string
%%% is an individual stock.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pageSelector(N) ->  inets:start(),PageNumber= lists:flatten(io_lib:format("~p", [N])),
{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
      httpc:request(string:concat("http://money.cnn.com/data/markets/nyse/?page=",PageNumber)), 
      {_,[[{Start,_}],[{Stop,_}]]} = re:run(Body,"tbody",[global]),
      StockData = string:substr(Body, Start+7, (Stop-Start)-8), 
      StockList = re:split(StockData,"<tr>",[{return,list}]),
      {Pno,_}=string:to_integer(PageNumber),io:format("Pageno ~p~n",[Pno]),
      loop({StockList,2,Pno}).

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
				_-> Dlist = lists:append(Datalist,[findWord(string:substr(List,N+1,M),List,N+1,M)]), findData(string:substr(List,N+1,M),List,N+1,M,Dlist)
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

-spec sendData([{atom(), any()}, ...]) -> ok.
sendData(SingleStockList) ->
	SingleStockList.
-spec loop(State :: any()) -> any().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Loops through the list of stocks from each page.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop({StockList,Number,PageNumber}) ->

case Number=<length(StockList) of
	true  -> ListSegment = lists:nth(Number,StockList),sendData(getData(findData(ListSegment,ListSegment,1,1,[]))),
	loop({StockList,Number+1,PageNumber});
	_ -> case PageNumber=<128 of
		% true -> pageSelector(PageNumber+1);
		_-> finished
	end
end.

