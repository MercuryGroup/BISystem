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

-define(STOCK, stock).
-define(MARKET, market).
-define(MARKETURL, "http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices.html?index=ASX").
-define(STOCKURL, "http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices-constituents.html?index=ASX&page=").
-define(USERAGENT, "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% start/0, A function for starting the extractor returns {ok, Pid}.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start()-> any()).
start()-> inets:start(), init(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/1, will recursivly spawn a new pageSelector for each page, i.e it will call itself recursivly 32 times. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init(pos_integer())->any()). 
init(33) -> ok; 

init(Count)-> 
  spawn(fun() -> pageSelector(Count) end),
  init(Count+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% pageSelector/1 takes a number representing a specific page, this page will be requested and then cut.
%%% returns ok
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(pageSelector(pos_integer())-> any()).
pageSelector(32) -> 
%% send request to the page
  {ok, {{_, _, _}, _, Body}} = httpc:request(get, {?MARKETURL, [{"User-Agent", ?USERAGENT}]}, [], []),

%% get chars between the regexes: "<tbody>" and "</tbody>"
  Regex_First = ["<tbody>"],
  Regex_Second = ["</tbody>"],
  {_,[{First_Value,_}]} = re:run(Body, Regex_First),
  {_,[{Second_Value,_}]} = re:run(Body, Regex_Second),
  Cut_List = lists:sublist(Body, First_Value, Second_Value-First_Value),
  getData(?MARKET, Cut_List);


pageSelector(Count) -> 
%% send request to the page
  PageUrl = lists:append(?STOCKURL, integer_to_list(Count)),
  {ok, {{_, _, _}, _, Body}} = httpc:request(get, {PageUrl, [{"User-Agent", ?USERAGENT}]}, [], []),

%% get chars between the regexes: "<tbody>" and "</tbody>"
  Regex_First = ["<tbody>"],
  Regex_Second = ["</tbody>"],
  {_,[{First_Value,_}]} = re:run(Body, Regex_First),
  {_,[{Second_Value,_}]} = re:run(Body, Regex_Second),
  Cut_List = lists:sublist(Body, First_Value, Second_Value-First_Value),

%% Split the individual stocks on the regex: "</tr>"
  Seperated_list = re:split(Cut_List, "</tr>", [{return, list}]),
  getData(?STOCK, Seperated_list).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% getData/2 takes a list HTML code with market or stock information and parses these, with the help of checkValues and formate, 
%%% after the parsing the formated market data will be sent to sendData/1.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(getData(atom(), list())-> any()).
getData(market, List) ->
%% get the market values from checkValues/2
  MarketValues = checkValues(List, []),
  case MarketValues of
%% if MarketValues equals [] we return []
    [] -> [];
%% otherwise we call formate/6 to get the market values formated and then we call sendData with that formated market values
    _  -> FormatedMarketValues = formate(?MARKET, MarketValues, 1, 0, 0, "GBX"),
          sendData(?MARKET, FormatedMarketValues)
  end;

%% getData/2 when the list only has one element left in it
getData(stock, [Head|[]]) ->
%% get the stock values from checkValues/2
  Stock = checkValues(Head, []),

  case Stock of

%% if Stock equals [] we return []
    [] -> [];

%% otherwise we remove the third element from the stock data, which represents the currency
%% and then we call formate/6 to get the stock formated and then we call sendData with that formated stock
    _  ->   Currrency = lists:nth(3, Stock), 
            NewStock = lists:delete(Currrency, Stock),
            FormatedStock = formate(?STOCK, NewStock, 1, 0, 0, Currrency),
            sendData(?STOCK, FormatedStock)
  end;

%% getData/2 when the list several elements left in it
getData(stock, [Head|Tail]) ->
%% get the stock values from checkValues/2
  Stock = checkValues(Head, []),
    case Stock of

%% if Stock equals [] we return []
      [] -> [];

%% otherwise we remove the third element from the stock data, which represents the currency
%% and then we call formate/6 to get the stock formated and then we call sendData with that formated stock
      _  -> Currrency = lists:nth(3, Stock), 
            NewStock = lists:delete(Currrency, Stock),
            FormatedStock = formate(?STOCK, NewStock, 1, 0, 0, Currrency),
            sendData(?STOCK, FormatedStock),
            getData(?STOCK, Tail)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% sendData/1 takes a formated stock and sends it to the transformer process. 
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(sendData(atom(), list())-> any()).
sendData(Tag, List)-> ?LOAD ! {Tag, List}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% formate/6 takes a list with market or stock data and formates it
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(formate(atom(), list(), integer(), list(), list(), list())-> list()).
formate(market, [Head|Tail], Count, Change, Current, Currrency) -> 
  case Count of

%% if the value of Count equals 1 we add the a tuple with latest as 
%% the tag and the formated latest value of the market in euro to the list
    1 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{latest, CurrencyInEUR} | formate(?MARKET, Tail, Count+1, Change, CurrencyInEUR, Currrency)];

%% if the value of Count equals 2 we add the a tuple with change as 
%% the tag and the formated value of the market change in euro to the list
    2 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{change, CurrencyInEUR} | formate(?MARKET, Tail, Count+1, CurrencyInEUR, Current, Currrency)];

%% if the value of Count equals 3 we add the a tuple with percent as 
%% the tag and the formated value of the percent to the list
    3 -> [{percent, formateNum(Head, [])} | formate(?MARKET, Tail, Count+1, Change, Current, Currrency)];

%% if the value of Count equals 4 we add the a tuple with highest as 
%% the tag and the formated value of the highest market value in euro to the list
    4 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{highest, CurrencyInEUR} | formate(?MARKET, Tail, Count+1, Change, Current, Currrency)];

%% if the value of Count equals 5 we add the a tuple with lowest as 
%% the tag and the formated value of the lowest market value in euro to the list
    5 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{lowest, CurrencyInEUR} | formate(?MARKET, Tail, Count+1, Change, Current, Currrency)];

%% if the value of Count equals 6 we add the a tuple with highest as 
%% the tag and the formated value of the highest market value in euro to the list
    6 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{closingVal, CurrencyInEUR} | formate(?MARKET, "null", Count+1, Change, Current, Currrency)];

%% if the value of Count equals 7 we add the a tuple with openVal as 
%% the tag and the return value of calc_opening/2 with our previous Change and Current values as arguments
%% after this we no longer call ourselfs with the tail of the original list since we no longer want 
%% elements from it
    7 -> [{openVal, calc_opening(Change, Current)} | formate(?MARKET, "null", Count+1,  "null","null", Currrency)];

%% if the value of Count equals 8 we add the a tuple with updated as 
%% the tag and the value of the defined func ?TIMESTAMP
    8 -> [{updated, ?TIMESTAMP} | formate(?MARKET, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 9 we add the a tuple with market as 
%% the tag and the and the "string" LSE, short for London Stock Exchange
    9 -> [{market, "LSE"} | formate(?MARKET, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 10 we add the a tuple with type as 
%% the tag and the and the "string" market
    10 -> [{type, "market"} | formate(?MARKET, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 11 we return the empty list
    11 -> [] 
  end;

formate(stock, [Head|Tail], Count, Change, Current, Currrency) -> 
  case Count of

%% if the value of Count equals 1 we add the a tuple with symbol as 
%% the tag and the Value of Head which here represents the Symbol of the stock
    1 -> [{symbol, Head} | formate(?STOCK, Tail, Count+1, Change, Current, Currrency)];

%% if the value of Count equals 2 we add the a tuple with name as 
%% the tag and the Value of Head which here represents the Name of the stock
    2 -> [{name, Head} | formate(?STOCK, Tail, Count+1, Change, Current, Currrency)];

%% if the value of Count equals 3 we add the a tuple with latest as 
%% the tag and the formated latest value of the stock in euro to the list
    3 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency), 
         [{latest, CurrencyInEUR} | formate(?STOCK, Tail, Count+1, Change, CurrencyInEUR, Currrency)];

%% if the value of Count equals 4 we add the a tuple with change as 
%% the tag and the formated value of the stocks change in euro to the list
    4 -> FormatedNumber = formateNum(Head, []),
         CurrencyInEUR = currencyWrapper(FormatedNumber, Currrency),  
         [{change, CurrencyInEUR} | formate(?STOCK, Tail, Count+1, CurrencyInEUR, Current, Currrency)];

%% if the value of Count equals 5 we add the a tuple with percent as 
%% the tag and the formated value of the stocks change in percent to the list
    5 -> [{percent, formateNum(Head, [])} | formate(?STOCK, "null", Count+1, Change, Current, Currrency)];

%% if the value of Count equals 6 we add the a tuple with openVal as 
%% the tag and the return value of calc_opening/2 with our previous Change and Current values as arguments
%% after this we no longer call ourselfs with the tail of the original list since we no longer want 
%% elements from it
    6 -> [{openVal, calc_opening(Change, Current)} | formate(?STOCK, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 7 we add the a tuple with updated as 
%% the tag and the value of the defined func ?TIMESTAMP
    7 -> [{updated, ?TIMESTAMP} | formate(?STOCK, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 8 we add the a tuple with market as 
%% the tag and the and the "string" LSE, short for London Stock Exchange
    8 -> [{market, "LSE"} | formate(?STOCK, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 9 we add the a tuple with type as 
%% the tag and the and the "string" stock
    9 -> [{type, "stock"} | formate(?STOCK, "null", Count+1, "null","null", Currrency)];

%% if the value of Count equals 10 we return the empty list
    10 -> []

  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% calc_opening/2 takes two lists of integers, Change and Current
%%% and returns the calculated opening value of the stock
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(calc_opening(list(), list())-> list()). 
calc_opening([ChangeHead|ChangeTail], Current) -> 
  case ChangeHead of 

%% if the first value of the "change-list" is minus, "-", we add the change to it to get the opening value
    $- -> Value = list_to_float(Current) + list_to_float(ChangeTail), 
          [StringValue] = io_lib:format("~.2f",[Value]),
          StringValue;   

%% if instead the value of "change-list" is plus, "+", we subtract the change to it to get the opening value
    $+ -> Value = list_to_float(Current) - list_to_float(ChangeTail), 
          [StringValue] = io_lib:format("~.2f",[Value]),
          StringValue;

%% Otherwise if its niether minus or plus, it expected to be zero, "0", and then we just return the Current variable
    _-> Current

  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% formateNum/2 Takes a list of integers and a empty Acc, 
%%% returns a list of integers with all spaces and "," removed.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(formateNum(list(), list())-> list()).
formateNum([], Accumulator) -> 
%% if the list is empty we return our Accumulator
  Accumulator; 

formateNum([$-|[]], _) ->
%% if we find a - sign we return the "string" "N/A"
  "N/A";

formateNum([Head|Tail], Accumulator) -> 
  case Head of

%% if we find a space we discard it
    $ -> formateNum(Tail, Accumulator);

%% if we find a comma we discard it
    $,-> formateNum(Tail, Accumulator);

%% otherwise we add the head to our Accumulator
    _-> formateNum(Tail, Accumulator ++ [Head])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% currencyWrapper/2 takes a list with the value aswell as the currency 
%%% and sends a request to the currency module, returns the value of the reply
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(currencyWrapper(list(), list())-> any()).
currencyWrapper([Head|Tail], Currency) ->

%% Beacause the values in our list sometimes start with either minus or plus we have to remove these
%% before we send it to the currency module and then add them again to our returning value, for simplicity we do 
%% the same thing with a zero
  if
    Head == $+ -> ?CURRENCY ! {request, self(), {Tail, Currency}},
      receive 
        {reply, Reply} -> lists:append([[$+], Reply]) 
      end;

    Head == $- -> ?CURRENCY ! {request, self(), {Tail, Currency}},
      receive 
        {reply, Reply} -> lists:append([[$-], Reply]) 
      end;

    true -> ?CURRENCY ! {request, self(), {[$0] ++ Tail, Currency}},
      receive 
        {reply, Reply} -> Reply 
      end
  end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% CeckValues/2 Traverses trought the html code and looks for the opening td tag, "<td"
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(checkValues(list(), list())-> list()).
checkValues([$<,$t,$d|Tail], Accumulator) ->
%% if we find a opening td tag we call getValues/1 with iterate of the Tail
  {NewValues,NewTail} = getValues(iterate(Tail), []),
  NewAccumulator = Accumulator ++ [NewValues],
  checkValues(NewTail, NewAccumulator);

checkValues([_|Tail], Accumulator) ->
%% if we don´t find a td tag we traverse
  checkValues(Tail, Accumulator);

checkValues([], Accumulator) ->
%% when the list is empty we return our Accumulator
  Accumulator.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% getValues/2 returns the stock values within the td tags
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(getValues(list(),list())->list()).
getValues([$<,$/,$t,$d,$>|Tail], Accumulator)->
%% if we find the ending td tag we return our Accumulator and the Tail
  {Accumulator,Tail};

getValues([$<|Tail], Accumulator)->
%% if we find a opening tag within the td tag we call ourselfs with iterate/1 on the tag
  getValues(iterate(Tail), Accumulator);

getValues([13|Tail], Accumulator)->
%% if we find row break we dicard it
  getValues(Tail, Accumulator);

getValues([10|Tail], Accumulator)->
%% if we find line break we dicard it
  getValues(Tail, Accumulator);

getValues([AnyValue|Tail], Accumulator)->
%% otherwise if none of the above clauses match we asume we´ve found the values 
%% we want and add them to the Accumulator
  NewAccumulator = Accumulator ++ [AnyValue],
  getValues(Tail, NewAccumulator).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% iterate/1, steps forward in the list until the next ">", returns the remaining html code
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(iterate(list())->list()).
iterate([$>|Tail]) ->
%% if we find a closing tag we return the tail
  Tail;

iterate([_|Tail]) ->
%% otherwise we keep travering the list
  iterate(Tail).
