%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: loadnyse.erl
%%% @author Rickard Bremer
%%% @doc
%%% Load stock data into our database.
%%% @end
%%% Created 14 October 2013 (Monday),09:00 by Rickard Bremer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(loadstocks).
-export([start/0, stop/0, init/0, loop/0, sendData/2, convert/1]).
-include("../include/ETL.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Start and register the process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() ->
	case whereis(?LOAD) of
		undefined ->
			register(?LOAD, spawn(?MODULE, init, [])),
				{ok, whereis(?LOAD)};
		_ ->
			{ok, whereis(?LOAD)}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Stop the process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
	case whereis(?LOAD) of
		undefined ->
			already_stopped;
		_ -> 
			exit(whereis(?LOAD), ok),
			stopped 
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Start all sub applications and enter the loop.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
	
	application:start(sasl),
	application:start(ibrowse),
	application:start(asn1),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(couchbeam),
	couchbeam:start(),

	loop().	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Pattern match the received list and upload it to the database.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData(_, []) ->
ok;

sendData(_Type, List) ->

%Data = e(List),
%io:format("~p \n", [Data]).

%[_Symbol|_List1] = StockList, {KeySymbol, ValSymbol} = _Symbol,
%[_Name|_List2] = _List1, {KeyName, ValName} = _Name,
%[_Change|_List3] = _List2, {KeyChange, ValChange} = _Change,
%[_Latest|_List4] = _List3, {KeyLatest, ValLatest} = _Latest,
%[_Percent|_List5] = _List4, {KeyPercent, ValPercent} = _Percent,
%[_Volume|_List6] = _List5, {KeyVolume, ValVolume} = _Volume,
%[_Time|_List7] = _List6, {KeyTime, ValTime} = _Time, 
%[_openinValue|_List8] = _List7, {KeyOpening, ValueOpening} = _openinValue,


%A = lists:append([atom_to_list(KeySymbol)," :"]),
 %<<"Symbol :">>, binary:list_to_bin(ValSymbol)},

	Server = couchbeam:server_connection("localhost", 5984, "", []),
	{ok, Db} = couchbeam:open_or_create_db(Server, ?DATABASE, []),	
%	Doc = {[{ binary:list_to_bin(A), binary:list_to_bin(ValSymbol)}, { <<"Name :">>, binary:list_to_bin(ValName)}, {<<"Change :" >>, binary:list_to_bin(ValChange)}, {<<"Latest :">>, binary:list_to_bin(ValLatest)}, {<<"Percent :">>, binary:list_to_bin(ValPercent)}, {<<"Volume :">>, binary:list_to_bin(ValVolume)}, {<<"Time :">>, binary:list_to_bin(ValTime)}, {<<"Opening Value :">>, binary:list_to_bin(ValueOpening) }]},
    
    Doc = { f(List)},
    
   io:format("~p",[Doc]),
    {ok, DocResult} = couchbeam:save_doc(Db, Doc),
    io:format("~p", [DocResult]).

convert([]) ->
	ok.

f([]) -> []; 
%	"[" ++ Acc ++ "]";

f([{Key, Val}|T]) ->
	[{binary:list_to_bin(atom_to_list(Key)), binary:list_to_bin(Val)}| f(T)]. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Receive a List from the Transform module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop() ->
	receive 
		{stock , List} ->
			sendData(stock, List);
		{market, List} ->
			sendData(market, List)
    end,
    loop().