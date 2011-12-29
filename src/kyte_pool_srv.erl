% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

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

-record(state, {
	pool_id :: integer(),
	pool_size :: integer(),
	affiliated_dbs = dict:new() :: dict(),
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
			}};
		OtherReply ->
			{stop, OtherReply}
	end.

handle_call({affiliate_db, DbPartSrv}, _From, State = #state{
	pool_id = PoolId,
	affiliated_dbs = Affiliated
}) ->
	MonRef = erlang:monitor(process, DbPartSrv),
	{reply, {ok, PoolId}, State#state{
		affiliated_dbs = dict:store(DbPartSrv, MonRef, Affiliated)
	}};

handle_call(shutdown, _From, State = #state{
	pool_id = PoolID,
	affiliated_dbs = Dbs
}) ->
	ok = stop_affiliated_dbs(Dbs),
	native_detroy_pool(PoolID),
	{stop, normal, ok, State #state{
		disposed = true
	}};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info( {'DOWN', _MonRef, process, DbPartSrv, _Reason}, State = #state{
	affiliated_dbs = Affiliated
} ) ->
	case dict:is_key(DbPartSrv, Affiliated) of
		true ->
			{noreply, State #state{
				affiliated_dbs = dict:erase(DbPartSrv, Affiliated)
			}};
		_ ->
			{noreply, State}
	end;

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, #state{
	pool_id = PoolID,
	affiliated_dbs = Dbs,
	disposed = false
}) ->
	ok = stop_affiliated_dbs(Dbs),
	native_detroy_pool(PoolID),
	ok;
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%% Internal

stop_affiliated_dbs(Dict) ->
	List = dict:to_list(Dict),
	lists:foreach(fun({DbPartSrv, MonRef}) ->
		erlang:demonitor(MonRef),
		kyte:db_partition_close_rude(DbPartSrv)
	end, List ),
	ok.

native_create_pool(PoolSize) ->
	kyte_nifs:create_thr_pool(PoolSize).

native_detroy_pool(PoolID) ->
	kyte_nifs:destroy_thr_pool(PoolID).

