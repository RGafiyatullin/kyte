% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_partition_srv).

-behaviour(gen_server).

-export([
	start_link/4
]).
-export([
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	part_id :: term(),
	pool :: pid(),
	handle :: undefined | {integer(), integer()},
	cookie = make_ref()
}).

start_link(ID, Pool, DbSrv, DbPath) ->
	gen_server:start_link(?MODULE, {ID, Pool, DbSrv, DbPath}, []).

init({ID, Pool, DbSrv, DbPath}) ->
	process_flag(trap_exit, true),
	{ok, PoolIdx} = gen_server:call(Pool, {affiliate_db, self()}, infinity),
	{ok, DbIdx} = kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_open(self(), Ref, PoolIdx, DbPath)
	end),
	kyte_db_srv:partition_init_notify(DbSrv, ID, self()),
	{ok, #state{
		pool = Pool,
		handle = {PoolIdx, DbIdx}
	}}.

handle_call({db_set, K, V}, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) 
when 
	is_binary(K) 
	and is_binary(V)
->
	kyte_nifs:db_set(self(), {Cookie, From}, PoolIdx, DbIdx, K, V),
	{noreply, State};

handle_call({db_get, K}, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) 
when 
	is_binary(K)
->
	kyte_nifs:db_get(self(), {Cookie, From}, PoolIdx, DbIdx, K),
	{noreply, State};

handle_call({db_remove, K}, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) 
when 
	is_binary(K)
->
	kyte_nifs:db_remove(self(), {Cookie, From}, PoolIdx, DbIdx, K),
	{noreply, State};

handle_call(db_count, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) ->
	kyte_nifs:db_count(self(), {Cookie, From}, PoolIdx, DbIdx),
	{noreply, State};

handle_call(db_size, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) ->
	kyte_nifs:db_size(self(), {Cookie, From}, PoolIdx, DbIdx),
	{noreply, State};

handle_call(db_clear, From, State = #state{ handle = {PoolIdx, DbIdx}, cookie = Cookie }) ->
	kyte_nifs:db_clear(self(), {Cookie, From}, PoolIdx, DbIdx),
	{noreply, State};

handle_call(db_close, _From, State = #state{ handle = {PoolIdx, DbIdx} }) ->
	io:format("part_srv:!db_close...~n", []),
	Ret = kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_close(self(), Ref, PoolIdx, DbIdx)
	end),
	io:format("part_srv:!db_close ~p~n", [Ret]),
	{stop, normal, ok, State # state{
		handle = undefined
	}};

handle_call(db_close_rude, _From, State = #state{ handle = {PoolIdx, DbIdx} }) ->
	_Ret = kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_close(self(), Ref, PoolIdx, DbIdx)
	end),
	{stop, rudely_closed, ok, State};

handle_call(Request, From, State = #state{}) ->
	io:format("~p got ~p from ~p~n", [self(), Request, From]),
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({ {Cookie, ReplyTo}, AsyncReply }, State = #state{ cookie = Cookie } ) ->
	gen_server:reply(ReplyTo, AsyncReply),
	{noreply, State};

handle_info({'EXIT', Pid, Reason}, State = #state{}) ->
	{stop, {killed, Pid, Reason}, State};

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(Reason, _State = #state{
	handle = undefined
}) ->
	io:format("[~p] kyte_db_partition: terminating with no handle (reason: ~p)~n", [self(), Reason]),
	ok;

terminate(Reason, _State = #state{
	handle = {PoolIdx, DbIdx}
}) ->
	% terminating
	io:format("[~p] kyte_db_partition_srv:terminate/3 (reason: ~p)~n", [self(), Reason]),
	Ret = kyte_nifs:execute_sync(fun(Ref) ->
		io:format("[~p] kyte_db_partition_srv:terminate/3 inside fun~n", [self()]),
		kyte_nifs:db_close(self(), Ref, PoolIdx, DbIdx)
	end),
	io:format("[~p] kyte_db_partition_srv:terminate/3 Ret: ~p~n", [self(), Ret]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

