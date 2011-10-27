-module(kyoto).

-export([
	start/0,
	stop/0,

	start_server/1,
	stop_server/1,

	open/2,
	close/1,

	clear/1,
	size/1,
	count/1,

	set/3,
	get/2,
	remove/2
]).

-include("KyotoPS.hrl").

-type server_handle() :: pid().
-type db_handle() :: {server_handle(), integer()}.


%%%
-spec start() -> any().
-spec stop() -> any().

start() -> application:start(kyoto_client).
stop() -> application:stop(kyoto_client).

%%% 
-spec start_server(Opts :: [{any(), any()}] ) -> {ok, Server :: pid()} | {error, any()}.
-spec stop_server(Server :: pid()) -> any().


start_server(Opts) ->
	io:format("Opts: ~p~n", [Opts]),
	{ok, Server} = supervisor:start_child(kc_port_srv_sup, [Opts]),
	erlang:link(Server),
	{ok, Server}.

stop_server(Pid) ->
	supervisor:terminate_child(kc_port_srv_sup, Pid),
	supervisor:delete_child(kc_port_srv_sup, Pid).


%%%
-spec open(Server :: pid(), FileName :: string()) -> {ok, db_handle()} | {error, any()}.
-spec close(db_handle()) -> ok | {error, any()}.

open(Srv, FileName) ->
	ReqType = 'KPSDbOpenRequest',
	RecType = 'KPSDbOpenRequest',
	Record = #'KPSDbOpenRequest' {
		dbFile = FileName
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSDbOpenResponse'{
			rc = 0,
			dbHandle = Handle
		} ->
			{ok, {Srv, Handle}};
		#'KPSDbOpenResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

close({Srv, Handle}) ->
	ReqType = 'KPSDbCloseRequest',
	RecType = 'KPSBasicRequest',
	Record = #'KPSBasicRequest'{
		dbHandle = Handle
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSBasicResponse'{
			rc = 0
		} ->
			ok;
		#'KPSBasicResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

size({Srv, Handle}) -> 
	ReqType = 'KPSDbSizeRequest',
	RecType = 'KPSBasicRequest',
	Record = #'KPSBasicRequest'{
		dbHandle = Handle
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSDbSizeResponse'{
			rc = 0,
			size = Size
		} ->
			{ok, Size};
		#'KPSDbSizeResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

count({Srv, Handle}) -> 
	ReqType = 'KPSDbCountRequest',
	RecType = 'KPSBasicRequest',
	Record = #'KPSBasicRequest'{
		dbHandle = Handle
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSDbCountResponse'{
			rc = 0,
			count = Count
		} ->
			{ok, Count};
		#'KPSDbCountResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

clear({Srv, Handle}) -> 
	ReqType = 'KPSDbClearRequest',
	RecType = 'KPSBasicRequest',
	Record = #'KPSBasicRequest'{
		dbHandle = Handle
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSBasicResponse'{
			rc = 0
		} ->
			ok;
		#'KPSBasicResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

set(Db, K, V) when is_list(K) ->
	K_as_B = list_to_binary(K),
	set(Db, K_as_B, V);
set(Db, K, V) when is_list(V) ->
	V_as_B = list_to_binary(V),
	set(Db, K, V_as_B);

set({Srv, Handle}, K, V) when is_binary(K) and is_binary(V) ->
	ReqType = 'KPSDbSetRequest',
	RecType = 'KPSDbSetRequest',
	Record = #'KPSDbSetRequest' {
		dbHandle = Handle,
		key = K,
		value = V
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSBasicResponse'{
			rc = 0
		} ->
			ok;
		#'KPSBasicResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

get(Db, K) when is_list(K) ->
	K_as_B = list_to_binary(K),
	?MODULE:get(Db, K_as_B);

get({Srv, Handle}, K) when is_binary(K) ->
	ReqType = 'KPSDbGetRequest',
	RecType = 'KPSDbGetRequest',
	Record = #'KPSDbGetRequest' {
		dbHandle = Handle,
		key = K
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSDbGetResponse'{
			rc = 0,
			value = Value
		} ->
			{ok, Value};
		#'KPSDbGetResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.

remove(Db, K) when is_list(K) ->
	K_as_B = list_to_binary(K),
	remove(Db, K_as_B);

remove({Srv, Handle}, K) when is_binary(K) ->
	ReqType = 'KPSDbRemoveRequest',
	RecType = 'KPSDbRemoveRequest',
	Record = #'KPSDbRemoveRequest' {
		dbHandle = Handle,
		key = K
	},
	case gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity) of
		#'KPSBasicResponse'{
			rc = 0
		} ->
			ok;
		#'KPSBasicResponse'{
			rc = RC,
			errorDescription = ED
		} ->
			{error, {rc, RC, ED}}
	end.


