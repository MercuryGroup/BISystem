%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: transform.erl
%%% @author Niklas Larsson
%%% @doc
%%% Transfom module for the whole ETL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(transform).
-export([start/0, stop/0]).
-include("ETL.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% start/0 - Registers the name and launches init/0.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start() -> {ok, pid()}).
start() ->
	case whereis(?TRANSFORM) of
		undefined ->
			register(?TRANSFORM, spawn(fun init/0)),
			start();
		Pid ->
			{ok, Pid}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/0 - Initialize the Transform.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stop() -> stopped).
stop() ->
	case whereis(?TRANSFORM) of 
		undefined ->
			stopped;
		_ ->
			?TRANSFORM ! {action, stop},
			stopped
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/0 - Initialize the Transform.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(init() -> ok).
init() ->
	PidList = spawnPids(?NUMBER_OF_TRANSFORM_HELPERS),
	mainLoop(PidList),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% init/0 - Initialize the Transform.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(spawnPids(Number :: pos_integer()) -> [pid(), ...]).
spawnPids(0) ->
	[];
spawnPids(Number) ->
	[spawn(fun loop/0) | spawnPids(Number - 1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% transform/1 - 	Iterates over the list of tuples and transforms the 
%%%					corresponding tuples in ?TUPPLES_TO_TRANSFORM
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(transform({Stock :: [{atom(), list()}, ...], atom()}) -> [{atom(), list()}, ...]).
transform({[], _}) ->
	[];
transform({[{Key, Value} | T], Currency}) ->
	case isin(Key, ?TUPPLES_TO_TRANSFORM) of
		true -> [{Key, currency_transformation(Currency, ?TO_CURRENCY, Value)} | transform({T, Currency})]; 
		false -> [{Key, Value} | transform({T, Currency})]
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% currency_transformation/1 -	Encapsulation to the currency converter.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIXME
-spec(currency_transformation(FromCurrency :: atom(), ToCurrency :: atom(), FloatAsString :: list()) -> any()).
currency_transformation(Currency, ToCurrency, Value) ->
	% Depending on how the currency converter works there might be a need to convert from string to a float
	% Talk to Justin about this
	Value * 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% sendData/1 - Sends the transformed Stock to ?LOAD.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(sendData(Tag :: atom(), Data :: [{atom(), list()}, ...]) -> ok).
sendData(Tag, Data) ->
	?LOAD ! {Tag, Data},
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% mainLoop/1 - Handles the data sent from various Extractors.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(mainLoop(PidList :: [pid(), ...]) -> ok).
mainLoop([PID | T]) ->
	receive 
		{stock, Stock} -> 
			PID ! {stock, Stock},
			NewState = T ++ [PID],
			mainLoop(NewState);
		{market, Market} ->
			PID ! {market, Market},
			NewState = T ++ [PID],
			mainLoop(NewState);

		{action, stop} ->
			stopAll([PID | T])
	end,
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% loop/1 - Handles the data sent from mainLoop/1.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIXME
-spec(loop() -> ok).
loop() ->
	receive
		{stock, Stock} ->
			ChangedStock = transform(Stock),
			sendData(stock, ChangedStock);

		{market, Market} ->
			ChangedMarket = transform(Market),
			sendData(market, ChangedMarket);

		{action, stop} ->
			ok
	end,
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% stopAll/1 - Stops all PIDs in the list.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(stopAll([pid(), ...]) -> ok).
stopAll([]) ->
	ok;
stopAll([PID | T]) ->
	PID ! {action, stop},
	stopAll(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% isin/2 - Checks if a element is in a list.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(isin(any(), [any(), ...]) -> boolean()).
isin(_, []) ->
	false;
isin(Element, [Element | _]) ->
	true;
isin(Element, [_ | T]) ->
	isin(Element, T).