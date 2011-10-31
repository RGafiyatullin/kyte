-module(kyte_db_srv).

-behaviour(gen_server).

-export([
	start_link/2
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
	handle :: undefined | {integer(), integer()},
	cookie = make_ref()
}).

start_link(PoolIdx, DbPath) ->
	gen_server:start_link(?MODULE, {PoolIdx, DbPath}, []).

init({PoolIdx, DbPath}) ->
	process_flag(trap_exit, true),
	io:format("init(~p)~n", [{PoolIdx, DbPath}]),
	ok = gen_server:call(kyte_pool_mgr, {affiliate_db, PoolIdx, self()}),

	{ok, DbIdx} = kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_open(self(), Ref, PoolIdx, DbPath)
	end),
	{ok, #state{
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

handle_call(db_close, _From, State = #state{}) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({ {Cookie, ReplyTo}, AsyncReply }, State = #state{ cookie = Cookie } ) ->
	gen_server:reply(ReplyTo, AsyncReply),
	{noreply, State};
	
handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{
	handle = {PoolIdx, DbIdx}
}) ->
	% terminating
	io:format("Terminating. Closing ~p~n", [{PoolIdx, DbIdx}]),
	kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_close(self(), Ref, PoolIdx, DbIdx)
	end),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

