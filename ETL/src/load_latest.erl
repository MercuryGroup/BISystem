-module(load_latest).
-compile(export_all).
-include("../include/ETL.hrl").

start() ->
	case whereis(?LOAD_LATEST) of
		undefined ->
			register(?LOAD_LATEST, spawn(fun init/0)),
			start();
		PID ->
			{ok, PID}
	end.

stop() ->
	case whereis(?LOAD_LATEST) of
		undefined ->
			already_stopped;
		_ ->
			?LOAD_LATEST ! {action, stop},
			stopped
	end.

init() ->
	application:start(sasl),
	application:start(ibrowse),
	application:start(asn1),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(couchbeam),
	couchbeam:start(),

	State = ets:new(rev, [set, named_table]),
	loop(State).


loop(State) ->
	receive
		{action, stop} ->
			ok;

		{stock, Stock} ->
			Id = get_symbol(Stock) ++ "." ++ get_market(Stock),
			%get rev from ets-table lookup(State, Id)
			%add rev and id to Stock
			case ets:lookup(State, Id) of
				[{_, Rev}] -> 
					Stock2 = [{'_rev', Rev} | Stock];

				_ ->
					Stock2 = Stock
			end,
			
			NewStock = [{'_id', Id} | Stock2],
			%send to database
			Server = couchbeam:server_connection(?DATABASE_HOSTNAME, ?DATABASE_PORT, "", []),

			Doc = {listToBin(NewStock)},
			% open new db connection
	        {ok, Db} = couchbeam:open_or_create_db(Server, ?DATABASE ++ "_latest", []),
	        % save couchbeam:save_doc(Db, NewDoc)
	        {ok, {Reply}} = couchbeam:save_doc(Db, Doc),

	        NewRev = get_rev(Reply),
	        %update rev in ets-table insert(State, {Id, rev})
	        ets:insert(State, {Id, NewRev}),
			loop(State);

		{action, reload} ->
			?MODULE:loop(State)

	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% get_type/1 - Returns the symbol.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_symbol([]) ->
	"";

get_symbol([{symbol, Val} | _]) ->
	Val;

get_symbol([_ | Tail]) ->
	get_symbol(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% get_type/1 - Returns the market.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_market([]) ->
	"";

get_market([{market, Val} | _]) ->
	Val;

get_market([_ | Tail]) ->
	get_market(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% get_type/1 - Returns the rev.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_rev([]) ->
	"";

get_rev([{<<"_rev">>, Val} | _]) ->
	binary:bin_to_list(Val);

get_rev([_ | Tail]) ->
	get_rev(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Turn List into a Bin.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listToBin([]) -> [];

listToBin([{Key, Val}|T]) ->
        [{unicode:characters_to_binary(atom_to_list(Key)), unicode:characters_to_binary(Val)}| listToBin(T)].