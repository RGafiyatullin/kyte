% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_srv).

-behaviour(gen_server).

-export([
	start_link/3
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
	parts_ctx :: term()
}).

start_link(Pool, PartsSup, Args) ->
	gen_server:start_link(?MODULE, {Pool, PartsSup, Args}, []).

init({Pool, PartsSup, Args = #kyte_db_args{
	file = DbFile,
	parts = DbPartsType
}}) ->
	%io:format("kyte_db_srv:init({~p, ~p})~n", [Pool, Args]),
	PartsCtx = kyte_parts:init(Pool, self(), PartsSup, DbFile, DbPartsType),
	{ok, #state{
		parts_sup = PartsSup,
		parts_ctx = PartsCtx
	}}.

handle_call(db_close, _From, State = #state{
	parts_ctx = PartsCtx
}) ->
	ok = kyte_parts:close_partitions(PartsCtx),
	{stop, normal, ok, State};

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


partition_init_notify(DbSrv, ID, PartSrv) ->
	gen_server:cast(DbSrv, {partition_init_notify, ID, PartSrv}).

