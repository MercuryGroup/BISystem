%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: lse_e.erl
%%% @author Alexander Ask
%%% @doc
%%% A htlm parser specially intended for the retriving stocks data, from the
%%% source: London stock exchange, http://www.londonstockexchange.com
%%% @end
%%% Created : 10 okt 2013 by Alexander Ask
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lse_e).
-export([start/0]).
%%-include("../include/ETL.hrl"). 
-include("ETL.hrl"). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% start/0, A function for starting the extractor returns {ok, Pid}.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()-> inets:start(), init(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/1, will recursivly spawn a new pageSelector for each page, i.e recurse 32 times. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(32) -> ok; 

init(N)-> 
  spawn(fun() -> pageSelector(N) end),
  init(N+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% pageSelector/1 takes a number representing a specific page, this page will be requested and then cut.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pageSelector(35) -> 
%% send request to the page
  UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0",
  Url = "http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices.html?index=ASX",
  {ok, {{_, _, _}, _, Body}} = httpc:request(get, {Url, [{"User-Agent", UserAgent}]}, [], []),

%% get chars between the regexes: "<tbody>" and "</tbody>"
  Regex_First = ["<tbody>"],
  Regex_Second = ["</tbody>"],
  {_,[{First_Value,_}]} = re:run(Body, Regex_First),
  {_,[{Second_Value,_}]} = re:run(Body, Regex_Second),
  Cut_List = lists:sublist(Body, First_Value, Second_Value-First_Value),
  getData(market, Cut_List);


pageSelector(N) -> 
%% send request to the page
  UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0",
  Url = "http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices-constituents.html?index=ASX&page=",
  PageUrl = lists:append(Url, integer_to_list(N)),
  {ok, {{_, _, _}, _, Body}} = httpc:request(get, {PageUrl, [{"User-Agent", UserAgent}]}, [], []),

%% get chars between the regexes: "<tbody>" and "</tbody>"
  Regex_First = ["<tbody>"],
  Regex_Second = ["</tbody>"],
  {_,[{First_Value,_}]} = re:run(Body, Regex_First),
  {_,[{Second_Value,_}]} = re:run(Body, Regex_Second),
  Cut_List = lists:sublist(Body, First_Value, Second_Value-First_Value),

%% Split the individual stocks on the regex: "</tr>"
  Seperated_list = re:split(Cut_List, "</tr>", [{return, list}]),
  getData(stock, Seperated_list).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% getData/2 takes a list HTML code with market or stock information and parses these, with the help of checkValues and formate, 
%%% after the parsing the formated market data will be sent to sendData/1.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getData(market, List) ->
  Stock = checkValues(List, []),
  case Stock of

    [] -> ok;
    _  -> FormatedStock = formate(market, Stock, 1, 0, 0),
          sendData(market, FormatedStock)
  end;

getData(stock, [H|[]]) ->
  Stock = checkValues(H, []),
  case Stock of

    [] -> ok;
    _  -> Cur = lists:nth(3, Stock), 
          NewStock = lists:delete(Cur, Stock),
          FormatedStock = formate(stock, NewStock, 1, 0, 0),
          sendData(stock, FormatedStock)
  end;

getData(stock, [H|T]) ->
  Stock = checkValues(H, []),
    case Stock of

      [] -> ok;

      _  -> Cur = lists:nth(3, Stock), 
            NewStock = lists:delete(Cur, Stock),
            FormatedStock = formate(stock, NewStock, 1, 0, 0),
            sendData(stock, FormatedStock),
            getData(stock, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% sendData/1 takes a formated stock and sends it to the transformer process. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData(Tag, List)-> 
io:format("~p~n", [{Tag, List}]).
%%?LOAD ! {Tag, List}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% formate/5 takes a list with market or stock data and formates it
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formate(market, [H|T], N, Change, Current) -> 
  case N of

    1 -> FormatedNumber = formateNum(H, []), 
    [{value, FormatedNumber} | formate(market, T, N+1, Change, FormatedNumber)];

    2 -> FormatedNumber = formateNum(H, []),
    [{change, FormatedNumber} | formate(market, T, N+1, FormatedNumber, Current)];

    3 -> [{percent, formateNum(H, [])} | formate(market, T, N+1, Change, Current)];

    4 -> [{highest, formateNum(H, [])} | formate(market, T, N+1, Change, Current)];

    5 -> [{lowest, formateNum(H, [])} | formate(market, T, N+1, Change, Current)];

    6 -> [{closingVal, formateNum(H, [])} | formate(market, "null", N+1, Change, Current)];

    7 -> [{openVal, calc_opening(Change, Current)} | formate(market, "null", N+1,  "","")];

    8 -> [{updated, ?TIMESTAMP} | formate(market, "null", N+1, "","")];

    9 -> [{market, "lse"} | formate(market, "null", N+1, "","")];

    10 -> [{type, "market"} | formate(market, "null", N+1, "","")];

    11 -> [] 

  end;

formate(stock, [H|T], N, Change, Current) -> 
  case N of

    1 -> [{symbol, H} | formate(stock, T, N+1, Change, Current)];

    2 -> [{name, H} | formate(stock, T, N+1, Change, Current)];

    3 -> FormatedNumber = formateNum(H, []), 
        [{latest, FormatedNumber} | formate(stock, T, N+1, Change, FormatedNumber)];

    4 -> FormatedNumber = formateNum(H, []), 
        [{change, FormatedNumber} | formate(stock, T, N+1, FormatedNumber, Current)];

    5 -> [{percent, formateNum(H, [])} | formate(stock, "null", N+1, Change, Current)];

    6 -> [{openVal, calc_opening(Change, Current)} | formate(stock, "null", N+1, "","")];

    7 -> [{updated, ?TIMESTAMP} | formate(stock, "null", N+1, "","")];

    8 -> [{market, "lse"} | formate(stock, "null", N+1, "","")];

    9 -> [{type, "stock"} | formate(stock, "null", N+1, "","")];

    10 -> []

  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Takes two lists of integers, Change and Current, and returns the calculated opening value of the stock
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_opening([ChangeHead|ChangeTail], Current) -> 
  case ChangeHead of 
    $- -> Value = list_to_float(Current) + list_to_float(ChangeTail), 
          Temp = float_to_list(Value),
          cut_decimal(Temp);

    $+ -> Value = list_to_float(Current) - list_to_float(ChangeTail), 
          Temp = float_to_list(Value),
          cut_decimal(Temp);

    $0 -> Current
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% cut_decimal/1 returns a lsit cut at the forth decimal. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cut_decimal([$.|T]) ->
  [$.|decimal(T, 4)];

cut_decimal([H|T]) -> 
  [H|cut_decimal(T)]. 

decimal([],_) ->
  [];

decimal(_,0)->
  [];

decimal([H|T], N) ->
  [H|decimal(T, N-1)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Takes a list of integers and returns a list of integers with all spaces and "," removed.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formateNum([], Acc) -> Acc;
%% JUSTINSMODLUE(Acc) ;

formateNum([H|T], Acc) -> 
  case H of
    $ -> formateNum(T, Acc);
    $,-> formateNum(T, Acc);
    _-> formateNum(T, Acc ++ [H])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Traverses trought the html code and looks for the opening td tag, "<td"
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkValues([$<,$t,$d|T], Acc) ->
  {{NewValues},Tail} = getValues(iterate(T), []),
  NewAcc = Acc ++ [NewValues],
  checkValues(Tail, NewAcc);

checkValues([_|T], Acc) ->
  checkValues(T, Acc);

checkValues([], Acc) ->
  Acc.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% returns the stock values within the td tags
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getValues([$<,$/,$t,$d,$>|T], Acc)->
  {{Acc},T};

getValues([$<|T], Acc)->
  getValues(iterate(T), Acc);

getValues([13|T], Acc)->
  getValues(T, Acc);

getValues([10|T], Acc)->
  getValues(T, Acc);

getValues([Val|T], Acc)->
  NewAcc = Acc ++ [Val],
  getValues(T, NewAcc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% iterates until the next ">", returns the remaining html code
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iterate([$>|T]) ->
  T;

iterate([_|T]) ->
  iterate(T).
