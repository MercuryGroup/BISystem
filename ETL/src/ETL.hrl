%%%----------------------------------------------
%%% ETL.hrl
%%% 2013-10-21
%%%
%%% The purpose of this file is to collect all 
%%% shared information in one place.
%%%
%%% NOTE: Changes to this file will affect several
%%%		  modules and should therefore only be
%%%		  done with the consensus of the group.
%%%----------------------------------------------

% This will hide the "function X is unsed" warning when compiling
-compile({nowarn_unused_function, [{milliseconds, 0}]}).

%%%----------------------------------------------
%%% CONSTANTS
%%%----------------------------------------------
-define(LOAD, stockLoader).						% Name of the Load
-define(TRANSFORM, transform).					% Name of the Transform
-define(TO_CURRENCY, eur).						% Currency we want to use as standard.
-define(TUPPLES_TO_TRANSFORM, [ latest,			% Tupples we want to transform the currency on.
								openingVal]).
-define(TIMESTAMP, milliseconds()).				% Milliseconds since the epoch (1970-01-01 00:00:00)
-define(NUMBER_OF_TRANSFORM_HELPERS, 10).		% The number of helper processes that should be spawned.

%%%----------------------------------------------
%%% FUNCTIONS
%%%----------------------------------------------
milliseconds() ->
	23.