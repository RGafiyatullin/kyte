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
	handle :: undefined | {integer(), integer()}
}).

start_link(PoolIdx, DbPath) ->
	gen_server:start_link(?MODULE, {PoolIdx, DbPath}, []).

init({PoolIdx, DbPath}) ->
	process_flag(trap_exit, true),
	io:format("init(~p)~n", [{PoolIdx, DbPath}]),

	{ok, DbIdx} = kyte_nifs:execute_sync(fun(Ref) ->
		kyte_nifs:db_open(self(), Ref, PoolIdx, DbPath)
	end),
	{ok, #state{
		handle = {PoolIdx, DbIdx}
	}}.

handle_call(db_close, _From, State = #state{}) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

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

