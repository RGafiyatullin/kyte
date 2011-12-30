% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte).

-export([start/0, stop/0]).
-export([pool_create/1, pool_destroy/1]).
-export([db_open/2, db_close/1, db_partition_close_rude/1]).
-export([db_set/3, db_del/2, db_get/2]).
-export([db_clear/1, db_size/1, db_count/1]).

-export([parts_post_hash_md5/1, parts_post_hash_sha/1]).

-include("kyte.hrl").

start() -> application:start(kyte).
stop() -> application:stop(kyte).

-spec pool_create( PoolSize ::integer() ) -> {ok, Pool :: pid()} | {error, any()}.
-spec pool_destroy( Pool :: pid() ) -> ok | {error, any()}.

-spec db_open( Pool :: pid(), kyte_db_args() ) -> {ok, DbSrv :: pid() }.
-spec db_close( DbSrv :: pid() ) -> ok.

-spec db_set( DbSrv :: pid(), K :: term(), V :: term() ) -> ok | {error, any()}.
-spec db_get( DbSrv :: pid(), K :: term() ) -> {ok, Value :: term()} | {error, any()}.
-spec db_del( DbSrv :: pid(), K :: term() ) -> ok | {error, any()}.

-spec db_count( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
-spec db_size( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
-spec db_clear( DbSrv :: pid() ) -> ok | {error, any()}.


%%% Pool operations

pool_create( PoolSize ) ->
	{ok, Pool} = supervisor:start_child(kyte_pool_sup, [PoolSize]),
	erlang:link(Pool),
	{ok, Pool}.


pool_destroy( Pool ) ->
	gen_server:call(Pool, shutdown, infinity).



%%% DB operations

db_open(Pool, Args = #kyte_db_args{}) ->
	{ok, _DbSrv} = supervisor:start_child( kyte_db_sup_sup, [ Pool, Args ] ).

db_close(DbSrv) ->
	gen_server:call(DbSrv, db_close, infinity).


db_partition_close_rude(DbPartSrv) ->
	case erlang:process_info(DbPartSrv) of
		undefined ->
			ok;
		_ ->
			gen_server:call(DbPartSrv, db_close_rude, infinity)
	end.

db_set(DbSrv, K, V) ->
	gen_server:call(DbSrv, {db_set, K, V}, infinity).

db_get(DbSrv, K) ->
	gen_server:call(DbSrv, {db_get, K}, infinity).

db_del(DbSrv, K) ->
	gen_server:call(DbSrv, {db_del, K}, infinity).

db_clear(DbSrv) ->
	gen_server:call(DbSrv, db_clear, infinity).

db_count(DbSrv) ->
	gen_server:call(DbSrv, db_count, infinity).

db_size(DbSrv) ->
	gen_server:call(DbSrv, db_size, infinity).


%%% Partitioning helper

parts_post_hash_md5(PartsCount) ->
	{post_hash, PartsCount, fun crypto:md5/1 }.

parts_post_hash_sha(PartsCount) ->
	{post_hash, PartsCount, fun crypto:sha/1 }.

%%% Internal

