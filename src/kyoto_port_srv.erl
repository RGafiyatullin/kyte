-module(kyoto_port_srv).
-behaviour(gen_server).

-export([
	start_link/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(kps_opt_db_file, 0).


-record(state, {
	port
}).

start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

init(Params) ->
	%process_flag(trap_exit, true),
	PortServerBin = proplists:get_value(port_server_binary, Params, "./kyoto-port-server"),
	Port = open_port({spawn, PortServerBin}, [{packet, 4}]),

	init_port_server_base(Port, Params),
	init_kyoto_port_server(Port, Params),

	receive
		{Port, {data, "open"}} ->
			{ok, #state{
				port = Port
			}};
		{Port, {data, Data}} ->
			io:format("kyoto_port_srv got ~p", [Data]),
			{stop, port_failure, #state{}}
	end.

	

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, State = #state{
	port = Port
}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send_packet(Port, Packet) ->
	Port ! {self(), {command, Packet}}.

send_nil_packet(Port) ->
	send_packet(Port, <<>>).

init_port_server_base(Port, _Params) ->
	send_nil_packet(Port).

init_kyoto_port_server(Port, Params) ->
	DBFile = proplists:get_value(db_file, Params),
	send_packet(Port, <<?kps_opt_db_file:8/integer, DBFile/binary>>),
	send_nil_packet(Port).

