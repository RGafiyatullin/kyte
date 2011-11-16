-module(kyte_pool_mgr).

-behaviour(gen_server).

-export([
	start_link/0
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
	pools = dict:new(),
	db2pool = dict:new()
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	process_flag(trap_exit, true),

	{ok, #state{}}.

handle_call({create_pool, PoolSize}, _From, State = #state{ pools = Pools }) ->
	case kyte_nifs:create_thr_pool(PoolSize) of
		{ok, PoolID} ->
			%NPools = sets:add_element(PoolID, Pools),
			NPools = dict:store(PoolID, sets:new(), Pools),
			{reply, {ok, PoolID}, State#state{ pools = NPools }};
		OtherReply ->
			{reply, OtherReply, State}
	end;

handle_call({destroy_pool, PoolID}, _From, State = #state{ pools = Pools, db2pool = DB2Pool }) ->
	case dict:find(PoolID, Pools) of
		error ->
			{reply, {error, enoent}, State};
		_ ->
			DBsSet = dict:fetch(PoolID, Pools),
			DBs = sets:to_list(DBsSet),
			shut_down_served_dbs(DBs),
			NDB2Pool = clean_db2pool(DBs, DB2Pool),
			NPools = dict:erase(PoolID, Pools),

			case kyte_nifs:destroy_thr_pool(PoolID) of
				ok ->
					%NPools = sets:del_element(PoolID, Pools),
					{reply, ok, State#state{ pools = NPools, db2pool = NDB2Pool }};
				OtherReply ->
					{reply, OtherReply, State}
			end
	end;

handle_call({affiliate_db, PoolID, DBSrv}, _From, State = #state{ pools = Pools, db2pool = DB2Pool }) ->
	erlang:monitor(process, DBSrv),
	case dict:find(PoolID, Pools) of
		{ok, Set} ->
			NSet = sets:add_element(DBSrv, Set),
			NPools = dict:store(PoolID, NSet, Pools),
			NDB2Pool = dict:store(DBSrv, PoolID, DB2Pool),

			{reply, ok, State#state{ pools = NPools, db2pool = NDB2Pool }};
		_ ->
			{reply, bad_pool_idx, State}
	end;

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info({'DOWN', _MonitorRef, _Type, DBSrv, _Info}, State = #state{ pools = Pools, db2pool = DB2Pool }) ->
	case dict:find(DBSrv, DB2Pool) of
		{ok, PoolID} ->
			NDB2Pool = dict:erase(DBSrv, DB2Pool),
			Set = dict:fetch(PoolID, Pools),
			NSet = sets:del_element(DBSrv, Set),
			NPools = dict:store(PoolID, NSet, Pools),

			{noreply, State#state{
				pools = NPools,
				db2pool = NDB2Pool
			}};
		_ ->
			{noreply, State}
	end;

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%% Internal

shut_down_served_dbs([]) ->
	ok;
shut_down_served_dbs([ Db | SoFar ]) ->
	try kyte:db_close_rude(Db) catch _:_ -> ok end,
	shut_down_served_dbs(SoFar).

clean_db2pool([], Db2Pool) ->
	Db2Pool;
clean_db2pool([ DbSrv | SoFar ], Db2Pool) ->
	clean_db2pool(SoFar, dict:erase(DbSrv, Db2Pool)).


