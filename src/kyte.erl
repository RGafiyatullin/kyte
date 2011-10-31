-module(kyte).

-export([start/0, stop/0]).
-export([pool_create/1, pool_destroy/1]).
-export([db_open/2, db_close/1]).

start() -> application:start(kyte).
stop() -> application:stop(kyte).

-spec pool_create( PoolSize ::integer() ) -> {ok, PoolID :: integer()} | {error, any()}.
pool_create(PoolSize) -> 
	gen_server:call(kyte_pool_mgr, {create_pool, PoolSize}, infinity).

-spec pool_destroy( PoolID :: integer() ) -> ok | {error, any()}.
pool_destroy(PoolID) ->
	gen_server:call(kyte_pool_mgr, {destroy_pool, PoolID}, infinity).

db_open(PoolIdx, DbFile) -> 
	{ok, DbSrv} = supervisor:start_child(kyte_db_sup, [PoolIdx, DbFile]),
	erlang:link(DbSrv),
	{ok, DbSrv}.

db_close(DbSrv) ->
	gen_server:call(DbSrv, db_close, infinity).