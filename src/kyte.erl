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
	supervisor:start_child(kyte_pool_sup, [PoolSize]).

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

db_set({DbSrv, Args}, K, V) ->
	Kt = encode_key(K, Args),
	Vt = encode_value(V, Args),
	gen_server:call(DbSrv, {db_set, Kt, Vt}, infinity).

db_get({DbSrv, Args}, K) ->
	Kt = encode_key(K, Args),
	case gen_server:call(DbSrv, {db_get, Kt}, infinity) of
		{ok, Vt} ->
			{ok, decode_value(Vt, Args)};
		Other ->
			Other
	end.

db_del({DbSrv, Args}, K) ->
	Kt = encode_key(K, Args),
	gen_server:call(DbSrv, {db_remove, Kt}, infinity).


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


% %%% Raw
% -spec db_set(pid(), bin_o_list(), bin_o_list()) -> ok | {error, any()}.
% db_set(DbSrv, K, V) when is_list(K) ->
% 	db_set(DbSrv, list_to_binary(K), V);
% db_set(DbSrv, K, V) when is_list(V) ->
% 	db_set(DbSrv, K, list_to_binary(V));
 
% db_set(DbSrv, K, V) when is_binary(K) and is_binary(V) ->
% 	gen_server:call(DbSrv, {db_set, K, V}, infinity).

% -spec db_remove(pid(), bin_o_list()) -> ok | {error, any()}.
% db_remove(DbSrv, K) when is_list(K) ->
% 	db_remove(DbSrv, list_to_binary(K));

% db_remove(DbSrv, K) when is_binary(K) ->
% 	gen_server:call(DbSrv, {db_remove, K}, infinity).

% -spec db_get(pid(), bin_o_list()) -> {ok, binary()} | {error, any()}.
% db_get(DbSrv, K) when is_list(K) ->
% 	db_get(DbSrv, list_to_binary(K));

% db_get(DbSrv, K) when is_binary(K) ->
% 	gen_server:call(DbSrv, {db_get, K}, infinity).



% %%% Term keys and values
% -spec db_xset(pid(), term(), term()) -> ok | {error, any()}.
% db_xset(DbSrv, Kt, Vt) ->
% 	Kb = sext:encode(Kt),
% 	Vb = term_to_binary(Vt),
% 	db_set(DbSrv, Kb, Vb).

% -spec db_xget(pid(), term()) -> {ok, term()} | {error, any()}.
% db_xget(DbSrv, Kt) ->
% 	Kb = sext:encode(Kt),
% 	case db_get(DbSrv, Kb) of
% 		{ok, Vb} ->
% 			{ok, binary_to_term(Vb)};
% 		Other ->
% 			Other
% 	end.

% -spec db_xremove(pid(), term()) -> ok | {error, any()}.
% db_xremove(DbSrv, Kt) ->
% 	Kb = sext:encode(Kt),
% 	db_remove(DbSrv, Kb).


% %%% Term keys, raw values
% -spec db_kset(pid(), term(), bin_o_list()) -> ok | {error, any()}.
% db_kset(DbSrv, Kt, Vb) when is_binary(Vb) or is_list(Vb) ->
% 	Kb = sext:encode(Kt),
% 	db_set(DbSrv, Kb, Vb).

% -spec db_kget(pid(), term()) -> {ok, term()} | {error, any()}.
% db_kget(DbSrv, Kt) ->
% 	Kb = sext:encode(Kt),
% 	db_get(DbSrv, Kb).

% -spec db_kremove(pid(), term()) -> ok | {error, any()}.
% db_kremove(DbSrv, Kt) ->
% 	db_xremove(DbSrv, Kt).

% %%% Zipped term keys and values
% -spec db_zset(pid(), term(), term()) -> ok | {error, any()}.
% db_zset(DbSrv, K, V) ->
% 	Kz = zlib:zip(term_to_binary(K)),
% 	Vz = zlib:zip(term_to_binary(V)),
% 	db_set(DbSrv, Kz, Vz).

% -spec db_zget(pid(), term()) -> {ok, term()} | {error, any()}.
% db_zget(DbSrv, K) ->
% 	Kz = zlib:zip(term_to_binary(K)),
% 	case db_get(DbSrv, Kz) of
% 		{ok, Vz} ->
% 			{ok, binary_to_term(zlib:unzip(Vz))};
% 		Other ->
% 			Other
% 	end.

% -spec db_zremove(pid(), term()) -> ok | {error, any()}.
% db_zremove(DbSrv, K) ->
% 	Kz = zlib:zip(term_to_binary(K)),
% 	db_remove(DbSrv, Kz).



-spec db_clear({pid(), kyte_db_args()}) -> ok | {error, any()}.
db_clear({DbSrv, _}) ->
	gen_server:call(DbSrv, db_clear, infinity).

-spec db_count({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_count({DbSrv, _}) ->
	gen_server:call(DbSrv, db_count, infinity).

-spec db_size({pid(), kyte_db_args()}) -> {ok, integer()} | {error, any()}.
db_size({DbSrv, _}) ->
	gen_server:call(DbSrv, db_size, infinity).
