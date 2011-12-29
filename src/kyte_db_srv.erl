% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_srv).

-behaviour(gen_server).

-export([
	start_link/2
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).
-export([
	partition_init_notify/3
]).

-include("kyte.hrl").

-record(state, {
	parts_sup :: pid(),
	parts_ctx :: term(),
	reqs_sup :: pid(),
	codecs :: {kyte_value_codec(), kyte_value_codec()}
}).

-spec start_link(pid(), kyte_db_args()) -> {ok, pid()}.

start_link(Pool, Args) ->
	gen_server:start_link(?MODULE, {Pool, Args}, []).

init({Pool, Args = #kyte_db_args{
	file = DbFile,
	parts = DbPartsType,
	key_codec = KCodec,
	val_codec = VCodec
}}) ->
	io:format("kyte_db_srv:init({~p, ~p})~n", [Pool, Args]),
	{ok, PartsSup} = kyte_db_partition_sup:start_link(),
	PartsCtx = kyte_parts:init(Pool, self(), PartsSup, DbFile, DbPartsType),
	{ok, ReqsSup} = kyte_db_request_sup:start_link(),
	io:format("kyte_db_srv inited~n"),
	{ok, #state{
		parts_sup = PartsSup,
		reqs_sup = ReqsSup,
		parts_ctx = PartsCtx,
		codecs = { KCodec, VCodec }
	}}.


handle_call(db_close, _From, State = #state{
	parts_ctx = PartsCtx
}) ->
	io:format("kyte_db_srv:!db_close 1~n", []),
	ok = kyte_parts:close_partitions(PartsCtx),
	io:format("kyte_db_srv:!db_close 2~n", []),
	{stop, normal, ok, State};

handle_call(Op = {db_set, _K, _V}, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};

handle_call(Op = {db_get, _K}, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};

handle_call(Op = {db_del, _K}, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};

handle_call(Op = db_clear, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};

handle_call(Op = db_count, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};

handle_call(Op = db_size, From, State = #state{ 
	parts_ctx = PartsCtx, 
	reqs_sup = ReqsSup,
	codecs = Codecs
}) ->
	perform_operation(ReqsSup, PartsCtx, Codecs, Op, From),
	{noreply, State};


handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({partition_init_notify, ID, PartSrv}, State = #state{
	parts_ctx = PartsCtx
}) ->
	{ ok, NewPartsCtx } = kyte_parts:partition_init_notify(PartsCtx, ID, PartSrv),
	{noreply, State #state{
		parts_ctx = NewPartsCtx
	}};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% API

partition_init_notify(DbSrv, ID, PartSrv) ->
	gen_server:cast(DbSrv, {partition_init_notify, ID, PartSrv}).

%%% Internal

perform_operation(Sup, PartsCtx, Codecs, Op, ReplyTo) ->
	{ok, ReqSrv} = supervisor:start_child(Sup, [PartsCtx, Codecs, Op, ReplyTo]),
	kyte_db_request_srv:execute(ReqSrv).

