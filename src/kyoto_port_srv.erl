-module(kyoto_port_srv).
-behaviour(gen_server).

-define(KyotoPortServerThreadPoolSize, 4).

-export([start_link/1]).

-export([
	open/2
]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("KyotoPS.hrl").

-define(kps_command_size_length, 4).

-define(kps_opt_end_of_opts, 0).
-define(kps_opt_thread_pool_size, 1).


-record(state, {
	port :: port(),
	replies = dict:new() :: dict()
}).

start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

init(Params) ->
	%process_flag(trap_exit, true),

	Port = start_port_server(Params),
	
	ok = init_port_server_base(Port, Params),
	ok = init_kyoto_port_server(Port, Params),

	{ok, #state{ port = Port }}.

handle_call({send_packet, ReqType, RecordType, Record}, From, State = #state{ port = Port, replies = Replies }) ->
	CommandId = kps_seq_srv:next(),
	PDUT = kps_pdu:pdu_type(ReqType),
	{ok, RecordEnc} = kps_pdu:encode(RecordType, Record),
	send_packet(Port, <<PDUT:16/little, CommandId:16/little, RecordEnc/binary>>),

	{noreply, State#state{
		replies = dict:store(CommandId, From, Replies)
	}}; % we'll reply later

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({Port, {data, Data}}, State = #state{ port = Port, replies = Replies }) ->
	<<PDUT:16/little, CommandId:16/little, Encoded/binary>> = list_to_binary(Data),
	{ok, Reply} = handle_response_pdu(PDUT, Encoded),
	ReplyTo = dict:fetch(CommandId, Replies),
	gen_server:reply(ReplyTo, Reply),

	{noreply, State#state{
		replies = dict:erase(CommandId, Replies)
	}};

handle_info({Port, {exit_status,0}}, State = #state{ port = Port }) ->
	% port server terminated gracefully
	{stop, normal, State};

handle_info({Port, {exit_status, RC}}, State = #state{ port = Port }) ->
	% whoops...
	{stop, {port_server_dead, RC}, State};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{
	port = _Port
}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internals

start_port_server(Params) ->
	PortServerBin = proplists:get_value(port_server_binary, Params, "./kyoto-port-server"),
	_Port = open_port({spawn, PortServerBin}, [{packet, ?kps_command_size_length}, exit_status]).

send_packet(Port, Packet) ->
	Port ! {self(), {command, Packet}}.

send_nil_packet(Port) ->
	send_packet(Port, <<>>).

init_port_server_base(Port, _Params) ->
	send_nil_packet(Port),
	ok.

init_kyoto_port_server(Port, Params) ->
	PDUT = kps_pdu:pdu_type('KPSSetOptionRequest'),

	CommandId1 = kps_seq_srv:next(),
	KPSThrPoolSize = proplists:get_value(thread_pool_size, Params, ?KyotoPortServerThreadPoolSize),
	SetThrPoolSize = #'KPSSetOptionRequest'{ 
		optCode = ?kps_opt_thread_pool_size,
		optValueInteger = KPSThrPoolSize
	},
	{ok, SetThrPoolSizeEnc} = kps_pdu:encode('KPSSetOptionRequest', SetThrPoolSize),
	send_packet(Port, <<PDUT:16/little, CommandId1:16/little, SetThrPoolSizeEnc/binary>>),
	
	CommandId2 = kps_seq_srv:next(),
	EndOfOpts = #'KPSSetOptionRequest' {
		optCode = ?kps_opt_end_of_opts
	},
	{ok, EndOfOptsEnc} = kps_pdu:encode('KPSSetOptionRequest', EndOfOpts),
	send_packet(Port, <<PDUT:16/little, CommandId2:16/little, EndOfOptsEnc/binary>>),

	ok.

handle_response_pdu(PDUT, Encoded) ->
	RecType = kps_pdu:rec_type(PDUT),
	
	{ok, _Decoded} = kps_pdu:decode(RecType, Encoded).

%% API

-spec open(KyotoPortSrv :: pid(), FileName :: binary()) -> {ok,DbHandle :: integer()} | {fail, RC :: integer(), Reason :: string()}.
open(Srv, FileName) ->
	ReqType = 'KPSDbOpenRequest',
	RecType = 'KPSDbOpenRequest',
	Record = #'KPSDbOpenRequest' {
		dbFile = FileName
	},
	gen_server:call(Srv, {send_packet, ReqType, RecType, Record}, infinity).

