%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: load.erl
%%% @author Rickard Bremer
%%% @doc
%%% Load stock data into our database.
%%% @end
%%% Created 14 October 2013 (Monday),09:00 by Rickard Bremer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(load).
-export([start/0, stop/0, init/0, loop/0, sendData/2]).
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
%%% Upload the Bin to the database.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendData(_, []) ->
ok;

sendData(_Type, List) ->
        
         case _Type of
         stock ->
                         %        Market = extractMarket(List),
                Market = string:to_lower(extractMarket(List)),
         ReMapped = reMappMarket(Market),
         Val = extractSymbol(List),
              
               case ReMapped of

                       "fail" -> ?NEWS ! {self(), symbol, Val};
                                    %io:format("~p~n", [Val]);
                _ -> ?NEWS ! {self(), symbol, Val ++ "." ++ ReMapped}
                                    %io:format("~p~n", [Val ++ "." ++ ReMapped])
                end;                        
         _ ->          ok
         end,
    
    Server = couchbeam:server_connection("localhost", 5984, "", []),
                {ok, Db} = couchbeam:open_or_create_db(Server, ?DATABASE, []),        
    
    Doc = { listToBin(List)},
       {ok, DocResult} = couchbeam:save_doc(Db, Doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Extract symbol from List
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractSymbol([]) -> [];

extractSymbol([{Key, Val}|T]) ->
        case Key of
                symbol -> Val;
                _ -> extractSymbol(T)
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Turn Extract market from List
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reMappMarket(Market) ->
    case Market of
                 "omx" -> "st";
                 "lse" -> "l";
                 "nyse" -> "fail";
                _ -> ok
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Turn Extract market from List
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extractMarket([]) -> [];

extractMarket([{Key, Val}|T]) ->
        case Key of
                market -> Val;
                _ -> extractMarket(T)
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Turn List into a Bin.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listToBin([]) -> [];

listToBin([{Key, Val}|T]) ->
        [{unicode:characters_to_binary(atom_to_list(Key)), unicode:characters_to_binary(Val)}| listToBin(T)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Receive a List from the Transform module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop() ->
        receive

                {stock , List} ->
                        sendData(stock, List),
                        ?LOAD_LATEST ! {stock, List},
                        loop();
                
                {market , List} ->
                        sendData(market, List),
                        loop();
                
                {news, List} ->
                      % io:format("~p~n", [List]),
                        spawn(?MODULE, sendData, [news, List]),
                        loop();
                {action, stop} -> ok;

                {action, reload} -> load:loop()

    end.