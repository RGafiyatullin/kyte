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

db_open(Pool, Args = #kyte_db_args{
	file = DbFile,
	parts = single
}) ->
	{ ok, DbSrv } = start_single_partition(Pool, DbFile),
	{ ok, {DbSrv, Args} };

db_open(Pool, Args = #kyte_db_args{
	file = DbFile,
	parts = { PreOrPost, PartsCount, _HF }
}) when (PreOrPost == pre_hash) or (PreOrPost == post_hash) ->
	Partitions = lists:map(fun(Idx) ->
		PartFile = lists:flatten(io_lib:format(DbFile, [Idx])),
		{ ok, DbSrv } = start_single_partition(Pool, PartFile),
		DbSrv
	end, lists:seq(1, PartsCount)),
	{ ok, {Partitions, Args} }.

db_close({DbSrv, #kyte_db_args{ parts = single }}) ->
	gen_server:call(DbSrv, db_close, infinity);

db_close({Parts, #kyte_db_args{ parts = { PreOrPost, _, _ } }}) when (PreOrPost == pre_hash) or (PreOrPost == post_hash) ->
	lists:foreach(fun(DbSrv) ->
		gen_server:call(DbSrv, db_close, infinity)
	end, Parts).


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
	gen_server:call(DbSrv, {db_set, Kt, Vt}, infinity);

db_set({Parts, Args = #kyte_db_args{ parts = { post_hash, PartsCount, HF } }}, K, V) ->
	PartsCount = length(Parts),

	Kt = encode_key(K, Args),
	Vt = encode_value(V, Args),
	{ok, DbSrv} = choose_partition(Kt, HF, Parts),
	gen_server:call(DbSrv, {db_set, Kt, Vt}, infinity).
	

db_get({DbSrv, Args = #kyte_db_args{ parts = single }}, K) ->
	Kt = encode_key(K, Args),
	case gen_server:call(DbSrv, {db_get, Kt}, infinity) of
		{ok, Vt} ->
			{ok, decode_value(Vt, Args)};
		Other ->
			Other
	end;

db_get({Parts, Args = #kyte_db_args{ parts = { post_hash, PartsCount, HF } }}, K) ->
	PartsCount = length(Parts),

	Kt = encode_key(K, Args),
	{ok, DbSrv} = choose_partition(Kt, HF, Parts),
	case gen_server:call(DbSrv, {db_get, Kt}, infinity) of
		{ok, Vt} ->
			{ok, decode_value(Vt, Args)};
		Other ->
			Other
	end.


db_del({DbSrv, Args = #kyte_db_args{ parts = single }}, K) ->
	Kt = encode_key(K, Args),
	gen_server:call(DbSrv, {db_remove, Kt}, infinity);

db_del({Parts, Args = #kyte_db_args{ parts = { post_hash, PartsCount, HF } }}, K) ->
	PartsCount = length(Parts),

	Kt = encode_key(K, Args),
	{ok, DbSrv} = choose_partition(Kt, HF, Parts),
	gen_server:call(DbSrv, {db_remove, Kt}, infinity).




-spec db_clear({pid(), kyte_db_args()}) -> ok | {error, any()}.
db_clear({DbSrv, #kyte_db_args{ parts = single }}) ->
	gen_server:call(DbSrv, db_clear, infinity);

db_clear({Parts, #kyte_db_args{ parts = { PreOrPost, _, _ } }}) when (PreOrPost == pre_hash) or (PreOrPost == post_hash) ->
	lists:foreach(fun(DbSrv) ->
		gen_server:call(DbSrv, db_clear, infinity)
	end, Parts).

-spec db_count({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_count({DbSrv, #kyte_db_args{ parts = single }}) ->
	gen_server:call(DbSrv, db_count, infinity);

db_count({Parts, #kyte_db_args{ parts = { PreOrPost, _, _ } }}) when (PreOrPost == pre_hash) or (PreOrPost == post_hash) ->
	Count = lists:foldl(fun(DbSrv, Acc) ->
		{ok, Count} = gen_server:call(DbSrv, db_count, infinity),
		Acc + Count
	end, 0, Parts),
	{ok, Count}.


-spec db_size({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_size({DbSrv, #kyte_db_args{ parts = single }}) ->
	gen_server:call(DbSrv, db_size, infinity);

db_size({Parts, #kyte_db_args{ parts = { PreOrPost, _, _ } }}) when (PreOrPost == pre_hash) or (PreOrPost == post_hash) ->
	Size = lists:foldl(fun(DbSrv, Acc) ->
		{ok, Size} = gen_server:call(DbSrv, db_size, infinity),
		Acc + Size
	end, 0, Parts),
	{ok, Size}.


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

choose_partition(K, HF, Parts) ->
	Kh = HF(K),
	Bits = size(Kh) * 8,
	<<Hash:Bits/unsigned>> = Kh,
	PartIdx = ( Hash rem length(Parts) ) + 1,
	{ok, lists:nth(PartIdx, Parts)}.

start_single_partition(Pool, DbFile) ->
	case supervisor:start_child(kyte_db_sup, [Pool, DbFile]) of
		{ok, DbSrv} ->
			erlang:link(DbSrv),
			{ok, DbSrv};
		OtherReply ->
			OtherReply
	end.

