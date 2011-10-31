-module(kyte_pool_mgr).

-export([]).

-behaviour(gen_server).

-export([
	start_link/0
]).
-export([
	create_pool/1,
	destroy_pool/1
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
	pools = sets:new()
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	process_flag(trap_exit, true),

	{ok, #state{}}.

handle_call({create_pool, PoolSize}, _From, State = #state{ pools = Pool }) ->
	case kyte_nifs:create_thr_pool(PoolSize) of
		{ok, PoolID} ->
			NPools = sets:add_element(PoolID, Pool),
			{reply, {ok, PoolID}, State#state{ pools = NPools }};
		OtherReply ->
			{reply, OtherReply, State}
	end;

handle_call({destroy_pool, PoolID}, _From, State = #state{ pools = Pool }) ->
	case kyte_nifs:destroy_thr_pool(PoolID) of
		ok ->
			NPools = sets:del_element(PoolID, Pool),
			{reply, ok, State#state{ pools = NPools }};
		OtherReply ->
			{reply, OtherReply, State}
	end;

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec create_pool(PoolSize :: integer()) -> {ok, integer()} | {error, any()}.
create_pool(PoolSize) ->
	gen_server:call(?MODULE, {create_pool, PoolSize}, infinity).

-spec destroy_pool(PoolID :: integer()) -> ok | {error, any()}.
destroy_pool(PoolID) ->
	gen_server:call(?MODULE, {destroy_pool, PoolID}, infinity).
