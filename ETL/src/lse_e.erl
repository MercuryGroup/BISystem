%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: lse_e.erl
%%% @author Alexander Ask
%%% @doc
%%% A htlm parser specially intended for the retreiving stocks data, from the
%%% source: London stock exchange, http://www.londonstockexchange.com
%%% @end
%%% Created : 10 okt 2013 by Alexander Ask
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lse_e).
-export([start/0]).
-include("../include/ETL.hrl").
 


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
init(33) -> ok; 

init(N)-> 
  spawn(fun() -> pageSelector(N) end),
  init(N+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% pageSelector/1 takes a number representing a specific page, this page will be requested and then cut.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pageSelector(32) -> 
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

    [] -> [];
    _  -> FormatedStock = formate(market, Stock, 1, 0, 0, "GBX"),
          sendData(market, FormatedStock)
  end;

getData(stock, [H|[]]) ->
  Stock = checkValues(H, []),
  case Stock of

    [] -> [];

    _  ->   Cur = lists:nth(3, Stock), 
            NewStock = lists:delete(Cur, Stock),
            FormatedStock = formate(stock, NewStock, 1, 0, 0, Cur),
            sendData(stock, FormatedStock)
  end;

getData(stock, [H|T]) ->
  Stock = checkValues(H, []),
    case Stock of

      [] -> [];

      _  -> Cur = lists:nth(3, Stock), 
            NewStock = lists:delete(Cur, Stock),
            %%WithOutIllegalSymbols = removeSymbols(NewStock, []),
            FormatedStock = formate(stock, NewStock, 1, 0, 0, Cur),
            sendData(stock, FormatedStock),
            getData(stock, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% sendData/1 takes a formated stock and sends it to the transformer process. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData(Tag, List)-> ?LOAD ! {Tag, List}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% formate/6 takes a list with market or stock data and formates it
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formate(market, [H|T], N, Change, Current, Cur) -> 
  case N of

    1 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{value, CurrencyInEUR} | formate(market, T, N+1, Change, CurrencyInEUR, Cur)];

    2 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{change, CurrencyInEUR} | formate(market, T, N+1, CurrencyInEUR, Current, Cur)];

    3 -> [{percent, formateNum(H, [])} | formate(market, T, N+1, Change, Current, Cur)];

    4 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{highest, CurrencyInEUR} | formate(market, T, N+1, Change, Current, Cur)];

    5 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{lowest, CurrencyInEUR} | formate(market, T, N+1, Change, Current, Cur)];

    6 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{closingVal, CurrencyInEUR} | formate(market, "null", N+1, Change, Current, Cur)];

    7 -> [{openVal, calc_opening(Change, Current)} | formate(market, "null", N+1,  "","", Cur)];

    8 -> [{updated, ?TIMESTAMP} | formate(market, "null", N+1, "","", Cur)];

    9 -> [{market, "LSE"} | formate(market, "null", N+1, "","", Cur)];

    10 -> [{type, "market"} | formate(market, "null", N+1, "","", Cur)];

    11 -> [] 

  end;

formate(stock, [H|T], N, Change, Current, Cur) -> 
  case N of

    1 -> [{symbol, H} | formate(stock, T, N+1, Change, Current, Cur)];

    2 -> [{name, H} | formate(stock, T, N+1, Change, Current, Cur)];

    3 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur), 
         [{latest, CurrencyInEUR} | formate(stock, T, N+1, Change, CurrencyInEUR, Cur)];

    4 -> FormatedNum = formateNum(H, []),
         CurrencyInEUR = currencyWrapper(FormatedNum, Cur),  
         [{change, CurrencyInEUR} | formate(stock, T, N+1, CurrencyInEUR, Current, Cur)];

    5 -> [{percent, formateNum(H, [])} | formate(stock, "null", N+1, Change, Current, Cur)];

    6 -> [{openVal, calc_opening(Change, Current)} | formate(stock, "null", N+1, "","", Cur)];

    7 -> [{updated, ?TIMESTAMP} | formate(stock, "null", N+1, "","", Cur)];

    8 -> [{market, "LSE"} | formate(stock, "null", N+1, "","", Cur)];

    9 -> [{type, "stock"} | formate(stock, "null", N+1, "","", Cur)];

    10 -> []

  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% calc_opening/2 takes two lists of integers, Change and Current
%%% and returns the calculated opening value of the stock
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_opening([ChangeHead|ChangeTail], Current) -> 
  case ChangeHead of 
    $- -> Value = list_to_float(Current) + list_to_float(ChangeTail), 
          [StringValue] = io_lib:format("~.2f",[Value]),
          StringValue;      

    $+ -> Value = list_to_float(Current) - list_to_float(ChangeTail), 
          [StringValue] = io_lib:format("~.2f",[Value]),
          StringValue;

    _-> Current

  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% formateNum/2 Takes a list of integers and a empty Acc, 
%%% returns a list of integers with all spaces and "," removed.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formateNum([], Acc) -> 
  Acc; 

formateNum([H|T], Acc) -> 
  case H of
    $ -> formateNum(T, Acc);
    $,-> formateNum(T, Acc);
    _-> formateNum(T, Acc ++ [H])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% currencyWrapper/2 takes a list with the value aswell as the currency 
%%% and sends a request to the currency module, returns the value of the reply
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
currencyWrapper([H|T], Cur) ->
  if
    H == $+ -> currency ! {request, self(), {T, Cur}},
      receive 
        {reply, Reply} -> lists:append([[$+], Reply]) 
      end;

    H == $- -> currency ! {request, self(), {T, Cur}},
      receive 
        {reply, Reply} -> lists:append([[$-], Reply]) 
      end;

    true -> currency ! {request, self(), {[$0] ++ T, Cur}},
      receive 
        {reply, Reply} -> Reply 
      end
  end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% CeckBalues/2 Traverses trought the html code and looks for the opening td tag, "<td"
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
%%% getValues/2 returns the stock values within the td tags
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
%%% iterate/1, steps forward in the list until the next ">", returns the remaining html code
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iterate([$>|T]) ->
  T;

iterate([_|T]) ->
  iterate(T).
