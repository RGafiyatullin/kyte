-module(kyoto_port_srv).
-behaviour(gen_server).

-define(KyotoPortServerThreadPoolSize, 4).

-export([start_link/1]).

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
	port
}).

start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

init(Params) ->
	%process_flag(trap_exit, true),

	Port = start_port_server(Params),
	
	ok = init_port_server_base(Port, Params),
	ok = init_kyoto_port_server(Port, Params),

	{ok, #state{ port = Port }}.

	

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

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

	KPSThrPoolSize = proplists:get_value(thread_pool_size, Params, ?KyotoPortServerThreadPoolSize),
	SetThrPoolSize = #'KPSSetOptionRequest'{ 
		commandId = kps_seq_srv:next(),
		optCode = ?kps_opt_thread_pool_size,
		optValueInteger = KPSThrPoolSize
	},
	{ok, SetThrPoolSizeEnc} = kps_pdu:encode('KPSSetOptionRequest', SetThrPoolSize),
	send_packet(Port, <<PDUT:16/little, SetThrPoolSizeEnc/binary>>),
	
	EndOfOpts = #'KPSSetOptionRequest' {
		commandId = kps_seq_srv:next(),
		optCode = ?kps_opt_end_of_opts
	},
	{ok, EndOfOptsEnc} = kps_pdu:encode('KPSSetOptionRequest', EndOfOpts),
	send_packet(Port, <<PDUT:16/little, EndOfOptsEnc/binary>>),

	ok.

