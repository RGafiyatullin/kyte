-module(kyoto).

% -export([
% 	start/0,
% 	stop/0,

% 	open/2,
% 	close/1,

% 	clear/1,
% 	size/1,
% 	count/1,

% 	set/3,
% 	get/2,
% 	remove/2
% ]).

-type pool_handle() :: integer().
-type db_handle() :: {pool_handle(), integer()}.

-export([start/0, stop/0]).
-export([pool_create/1, pool_destroy/1]).
-export([db_open/2, db_close/1]).
-export([db_get/2, db_set/3, db_remove/2]).

%%%
start() -> application:start(kyoto_client).
stop() -> application:stop(kyoto_client).

%%% 
-spec pool_create(Size :: integer()) -> {ok, pool_handle()} | {error, any()}.
pool_create(Size) ->
	kyoto_nifs:create_thr_pool(Size).

-spec pool_destroy(Pool :: pool_handle()) -> ok | {error, any()}.
pool_destroy(Pool) ->
	%{error, will_crash}.
	kyoto_nifs:destroy_thr_pool(Pool).

sync_command(F) ->
	Ref = make_ref(),
	case F(Ref) of
		ok ->
			receive
				{Ref, Reply} ->
					Reply
			end;
		InstantReply ->
			InstantReply
	end.

-spec db_open(Pool :: pool_handle(), File :: string() ) -> {ok, db_handle()} | {error, any()}.
db_open(Pool, File)  when is_integer(Pool) ->
	sync_command( fun(Ref) -> kyoto_nifs:db_open(self(), Ref, Pool, File) end ).
	
db_close({PoolIdx, DbIdx}) when is_integer(PoolIdx) and is_integer(DbIdx) ->
	sync_command( fun(Ref) -> kyoto_nifs:db_close(self(), Ref, PoolIdx, DbIdx) end ).

db_set(Db, K, V) when is_list(K) ->
	db_set(Db, list_to_binary(K), V);
db_set(Db, K, V) when is_list(V) ->
	db_set(Db, K, list_to_binary(V));

db_set({PoolIdx, DbIdx}, K, V) when is_integer(PoolIdx) and is_integer(DbIdx) and is_binary(K) and is_binary(V) ->
	sync_command( fun(Ref) -> kyoto_nifs:db_set(self(), Ref, PoolIdx, DbIdx, K, V) end ).

db_get(Db, K) when is_list(K) ->
	db_get(Db, list_to_binary(K));

db_get({PoolIdx, DbIdx}, K) when is_integer(PoolIdx) and is_integer(DbIdx) and is_binary(K) ->
	sync_command( fun(Ref) -> kyoto_nifs:db_get(self(), Ref, PoolIdx, DbIdx, K) end ).

db_remove({PoolIdx, DbIdx}, K) when is_integer(PoolIdx) and is_integer(DbIdx) and is_binary(K) ->
	sync_command( fun(Ref) -> kyoto_nifs:db_remove(self(), Ref, PoolIdx, DbIdx, K) end ).
