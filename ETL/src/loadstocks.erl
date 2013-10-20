%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: loadnyse.erl
%%% @author Rickard Bremer
%%% @doc
%%% Load stock data into our database.
%%% @end
%%% Created 14 October 2013 (Monday),09:00 by Rickard Bremer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(loadstocks).
-export([start/0, stop/0, init/0, loop/0, sendData/1, convert/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Start and register the process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> {ok, pid()}.
start() ->
	case whereis(lstd) of
		undefined ->
			register(lstd, spawn(loadstocks, init, [])),
				{ok, whereis(lstd)};
		_ ->
			{ok, whereis(lstd)}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Stop the process.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
	case whereis(lstd) of
		undefined ->
			already_stopped;
		_ -> 
			exit(whereis(lstd), ok),
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
sendData([]) ->
ok;

sendData(StockList) ->

[_Symbol|_List1] = StockList, {_KeySymbol, ValSymbol} = _Symbol,

[_Name|_List2] = _List1, {_KeyName, ValName} = _Name,

[_Change|_List3] = _List2, {_KeyChange, ValChange} = _Change,

[_Latest|_List4] = _List3, {_KeyLatest, ValLatest} = _Latest,

[_Percent|_List5] = _List4, {_KeyPercent, ValPercent} = _Percent,

[_Volume|_List6] = _List5, {_KeyVolume, ValVolume} = _Volume,

[_Time|_List7] = _List6, {_KeyTime, ValTime} = _Time, 

[_openinValue|_List8] = _List7, {_KeyOpening, ValueOpening} = _openinValue,

	Server = couchbeam:server_connection("localhost", 5984, "", []),
	{ok, Db} = couchbeam:open_or_create_db(Server, "testdb", []),	
	Doc = {[ {<<"Symbol :">>, binary:list_to_bin(ValSymbol)},{ <<"Name :">>, binary:list_to_bin(ValName)}, {<<"Change :" >>, binary:list_to_bin(ValChange)}, {<<"Latest :">>, binary:list_to_bin(ValLatest)}, {<<"Percent :">>, binary:list_to_bin(ValPercent)}, {<<"Volume :">>, binary:list_to_bin(ValVolume)}, {<<"Time :">>, binary:list_to_bin(ValTime)}, {<<"Opening Value :">>, binary:list_to_bin(ValueOpening) }]},
    io:format("~p",[Doc]),
    {ok, DocResult} = couchbeam:save_doc(Db, Doc),
    io:format("~p", [DocResult]).

convert([]) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Receive a List from the Transform module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop() ->
	receive 
		List ->
			sendData(List)
    end,
    loop().