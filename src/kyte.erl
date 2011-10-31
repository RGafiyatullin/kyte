-module(kyte).

-export([start/0, stop/0]).
-export([pool_create/1, pool_destroy/1]).
-export([db_open/2, db_close/1]).
-export([db_set/3, db_remove/2, db_get/2]).
-export([db_clear/1, db_size/1, db_count/1]).

-type bin_o_list() :: binary() | [ integer() ].

start() -> application:start(kyte).
stop() -> application:stop(kyte).

-spec pool_create( PoolSize ::integer() ) -> {ok, PoolID :: integer()} | {error, any()}.
pool_create(PoolSize) -> 
	gen_server:call(kyte_pool_mgr, {create_pool, PoolSize}, infinity).

-spec pool_destroy( PoolID :: integer() ) -> ok | {error, any()}.
pool_destroy(PoolID) ->
	gen_server:call(kyte_pool_mgr, {destroy_pool, PoolID}, infinity).

db_open(PoolIdx, DbFile) -> 
	case supervisor:start_child(kyte_db_sup, [PoolIdx, DbFile]) of
		{ok, DbSrv} ->
			erlang:link(DbSrv),
			{ok, DbSrv};
		OtherReply ->
			OtherReply
	end.

db_close(DbSrv) ->
	gen_server:call(DbSrv, db_close, infinity).


-spec db_set(pid(), bin_o_list(), bin_o_list()) -> ok | {error, any()}.
db_set(DbSrv, K, V) when is_list(K) ->
	db_set(DbSrv, list_to_binary(K), V);
db_set(DbSrv, K, V) when is_list(V) ->
	db_set(DbSrv, K, list_to_binary(V));
 
db_set(DbSrv, K, V) when is_binary(K) and is_binary(V) ->
	gen_server:call(DbSrv, {db_set, K, V}, infinity).

-spec db_remove(pid(), bin_o_list()) -> ok | {error, any()}.
db_remove(DbSrv, K) when is_list(K) ->
	db_remove(DbSrv, list_to_binary(K));

db_remove(DbSrv, K) when is_binary(K) ->
	gen_server:call(DbSrv, {db_remove, K}, infinity).

-spec db_get(pid(), bin_o_list()) -> {ok, binary()} | {error, any()}.
db_get(DbSrv, K) when is_list(K) ->
	db_get(DbSrv, list_to_binary(K));

db_get(DbSrv, K) when is_binary(K) ->
	gen_server:call(DbSrv, {db_get, K}, infinity).

-spec db_clear(pid()) -> ok | {error, any()}.
db_clear(DbSrv) ->
	gen_server:call(DbSrv, db_clear, infinity).

-spec db_count(pid()) -> {ok, integer()} | {error, any()}.
db_count(DbSrv) ->
	gen_server:call(DbSrv, db_count, infinity).

-spec db_size(pid()) -> {ok, integer()} | {error, any()}.
db_size(DbSrv) ->
	gen_server:call(DbSrv, db_size, infinity).

