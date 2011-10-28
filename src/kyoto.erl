-module(kyoto).

-export([
	start/0,
	stop/0,

	open/2,
	close/1,

	clear/1,
	size/1,
	count/1,

	set/3,
	get/2,
	remove/2
]).

-type server_handle() :: pid().
-type db_handle() :: {server_handle(), integer()}.

%%%
-spec start() -> any().
-spec stop() -> any().

start() -> application:start(kyoto_client).
stop() -> application:stop(kyoto_client).

%%% 

%%%
-spec open(Server :: pid(), FileName :: string()) -> {ok, db_handle()} | {error, any()}.
-spec close(db_handle()) -> ok | {error, any()}.

open(_ThrPool, _FileName) ->
	{error, not_implemented}.

close(_Db) ->
	{error, not_implemented}.

size(_Db) -> 
	{error, not_implemented}.

count(_Db) -> 
	{error, not_implemented}.

clear(_Db) -> 
	{error, not_implemented}.

set(Db, K, V) when is_list(K) ->
	K_as_B = list_to_binary(K),
	set(Db, K_as_B, V);
set(Db, K, V) when is_list(V) ->
	V_as_B = list_to_binary(V),
	set(Db, K, V_as_B);

set(_Db, K, V) when is_binary(K) and is_binary(V) ->
	{error, not_implemented}.

get(Db, K) when is_list(K) ->
	K_as_B = list_to_binary(K),
	?MODULE:get(Db, K_as_B);

get(_Db, K) when is_binary(K) ->
	{error, not_implemented}.

remove(Db, K) when is_list(K) ->
	K_as_B = list_to_binary(K),
	remove(Db, K_as_B);

remove(_Db, K) when is_binary(K) ->
	{error, not_implemented}.


