%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ETL.hrl
%%% 2013-10-21
%%%
%%% The purpose of this file is to collect all 
%%% shared information in one place.
%%%
%%% NOTE: 	Changes to this file will affect several modules and should 
%%% 		therefore only be done with the consensus of the group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This will hide the "function X is unsed" warning when compiling
-compile({nowarn_unused_function, [{milliseconds, 0}]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(LOAD, stockLoader).								% Name of the Load
-define(CURRENCY, currency).							% Name of the Currency Converter			
-define(TO_CURRENCY, "eur").							% Currency we want to use as standard.
-define(TIMESTAMP, integer_to_list(milliseconds())).	% Milliseconds since the epoch (1970-01-01 00:00:00)
-define(DATABASE, "mercury").							% Name of the database
-define(ETL, etl).										% Name of the ETL-controller
-define(SCHEDULER, schedule).							% Name of the scheduler
-define(UPDATE_INTERVAL, 60000).						% Update interval for the scheduler, time is in milliseconds
-define(LAUNCH_LIST, [									% The default configuration for the scheduler
	{nyse, fun nyse_e:start/0, [{14,45}, {18,45}, {22, 45}], ?ETL},
	{omx, fun omx_e:start/0, [{9,05}, {13,15}, {17,25}], ?ETL},
	{lse, fun lse_e:start/0, [{10,0}, {13,30}, {17,0}], ?ETL}%,
	%{news, fun newsrss_e:start/0, [{0,05}], ?ETL}
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%	milliseconds/0 - Returns the timestamp we want
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(milliseconds() -> pos_integer()).
milliseconds() ->
	{Z,X,Y} = now(), 
	(((Z*1000000)+X)*1000)+Y.