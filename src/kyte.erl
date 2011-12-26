-module(kyte).

-export([start/0, stop/0]).
-export([pool_create/1, pool_destroy/1]).
-export([db_open/2, db_close/1, db_close_rude/1]).
-export([db_set/3, db_del/2, db_get/2]).
-export([db_clear/1, db_size/1, db_count/1]).

-include("kyte.hrl").

start() -> application:start(kyte).
stop() -> application:stop(kyte).

-spec pool_create( PoolSize ::integer() ) -> {ok, Pool :: pid()} | {error, any()}.
pool_create( PoolSize ) ->
	{ok, Pool} = supervisor:start_child(kyte_pool_sup, [PoolSize]),
	erlang:link(Pool),
	{ok, Pool}.

-spec pool_destroy( Pool :: pid() ) -> ok | {error, any()}.
pool_destroy( Pool ) ->
	gen_server:call(Pool, shutdown, infinity).

db_open(PoolIdx, Args = #kyte_db_args{
	file = DbFile
}) ->
	case supervisor:start_child(kyte_db_sup, [PoolIdx, DbFile]) of
		{ok, DbSrv} ->
			erlang:link(DbSrv),
			{ok, {DbSrv, Args}};
		OtherReply ->
			OtherReply
	end.

db_close({DbSrv, _}) ->
	gen_server:call(DbSrv, db_close, infinity).

db_close_rude({DbSrv, _}) ->
	case erlang:process_info(DbSrv) of
		undefined ->
			ok;
		_ ->
			gen_server:call(DbSrv, db_close_rude, infinity)
	end.


-spec db_set({pid(), kyte_db_args()}, any(), any()) -> ok | {error, any()}.
-spec db_get({pid(), kyte_db_args()}, any()) -> {ok, term()} | {error, any()}.
-spec db_del({pid(), kyte_db_args()}, any()) -> ok | {error, any()}.

db_set({DbSrv, Args = #kyte_db_args{ parts = single }}, K, V) ->
	Kt = encode_key(K, Args),
	Vt = encode_value(V, Args),
	gen_server:call(DbSrv, {db_set, Kt, Vt}, infinity).
	
db_get({DbSrv, Args = #kyte_db_args{ parts = single }}, K) ->
	Kt = encode_key(K, Args),
	case gen_server:call(DbSrv, {db_get, Kt}, infinity) of
		{ok, Vt} ->
			{ok, decode_value(Vt, Args)};
		Other ->
			Other
	end.

db_del({DbSrv, Args = #kyte_db_args{ parts = single }}, K) ->
	Kt = encode_key(K, Args),
	gen_server:call(DbSrv, {db_remove, Kt}, infinity).




-spec db_clear({pid(), kyte_db_args()}) -> ok | {error, any()}.
db_clear({DbSrv, _}) ->
	gen_server:call(DbSrv, db_clear, infinity).

-spec db_count({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_count({DbSrv, _}) ->
	gen_server:call(DbSrv, db_count, infinity).

-spec db_size({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_size({DbSrv, _}) ->
	gen_server:call(DbSrv, db_size, infinity).



%%% Internal
encode_key(K, #kyte_db_args{
	key_codec = C
}) ->
	encode_with(C, K).

encode_value(V, #kyte_db_args{
	val_codec = C
}) ->
	encode_with(C, V).

encode_with(C, V) ->
	case C of
		raw ->
			V;
		rawz ->
			zlib:zip(V);
		etf ->
			term_to_binary(V);
		etfz ->
			zlib:zip(term_to_binary(V));
		sext ->
			sext:encode(V)
	end.

decode_value(V, #kyte_db_args{
	val_codec = C
}) ->
	decode_with(C, V).

decode_with(C, V) ->
	case C of
		raw ->
			V;
		rawx ->
			zlib:unzip(V);
		etf ->
			binary_to_term(V);
		etfz ->
			binary_to_term(zlib:unzip(V));
		sext ->
			sext:decode(V)
	end.



