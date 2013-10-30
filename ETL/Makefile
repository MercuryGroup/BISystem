etl: ebin/currency.beam ebin/etl.beam ebin/load.beam ebin/lse_e.beam ebin/newsrss_e.beam ebin/nyse_e.beam ebin/omx_e.beam ebin/scheduler.beam include/ETL.hrl



ebin/currency.beam: src/currency.erl include/ETL.hrl
	erlc -o ebin src/currency.erl

ebin/etl.beam: src/etl.erl include/ETL.hrl
	erlc -o ebin src/etl.erl

ebin/load.beam: src/load.erl include/ETL.hrl
	erlc -o ebin src/load.erl

ebin/lse_e.beam: src/lse_e.erl include/ETL.hrl
	erlc -o ebin src/lse_e.erl

ebin/newsrss_e.beam: src/newsrss_e.erl include/ETL.hrl
	erlc -o ebin src/newsrss_e.erl

ebin/nyse_e.beam: src/nyse_e.erl include/ETL.hrl
	erlc -o ebin src/nyse_e.erl

ebin/omx_e.beam: src/omx_e.erl include/ETL.hrl
	erlc -o ebin src/omx_e.erl

ebin/scheduler.beam: src/scheduler.erl include/ETL.hrl
	erlc -o ebin src/scheduler.erl
