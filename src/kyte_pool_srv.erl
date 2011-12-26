-module(kyte_pool_srv).

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

-include("logging.hrl").

-record(state, {
	pool_id :: integer(),
	pool_size :: integer(),
	disposed = false
}).

start_link(PoolSize) ->
	gen_server:start_link(?MODULE, {PoolSize}, []).

init({PoolSize}) ->
	process_flag(trap_exit, true),
	case native_create_pool(PoolSize) of
		{ok, PoolID} ->
			{ok, #state{
				pool_id = PoolID,
				pool_size = PoolSize
			}}.
		OtherReply ->
			{stop, OtherReply}
	end.

handle_call({open_db, DbPath}, _From, State = #state{}) ->
	{stop, {error, not_impl}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{
	pool_id = PoolID,
	disposed = false
}) ->
	native_detroy_pool(PoolID),
	ok;
terminate(_Reason, _State) ->
	ok.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%% Internal

native_create_pool(PoolSize) ->
	kyte_nifs:create_thr_pool(PoolSize).

native_detroy_pool(PoolID) ->
	ok.

