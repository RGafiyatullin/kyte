-module(kyte_db_request_srv).

-behaviour(gen_server).

-export([
	start_link/3,
	execute/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-type kyte_db_request_operation() ::
	{db_set, Key :: term(), Value :: term()} |
	{db_get, Key :: term()} |
	{db_del, Key :: term()} |
	db_size |
	db_count |
	db_clear.


-record(state, {
	operation :: kyte_db_request_operation(),
	reply_to :: {pid(), reference()},
	parts_ctx :: term()
}).

start_link(PartsCtx, Operation, ReplyTo) ->
	gen_server:start_link(?MODULE, {PartsCtx, Operation, ReplyTo}, []).

init({PartsCtx, Operation, ReplyTo}) ->
	{ok, #state{
		operation = Operation,
		reply_to = ReplyTo,
		parts_ctx = PartsCtx
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(execute, State = #state{
	operation = Op,
	reply_to = ReplyTo,
	parts_ctx = PartsCtx
}) ->
	{LinkTo, _} = ReplyTo,
	erlang:link(LinkTo),
	ok = perform_execute(Op, ReplyTo, PartsCtx),
	{stop, normal, State};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% API

execute(ReqSrv) ->
	gen_server:cast(ReqSrv, execute).

%%% Internal

parts_fold(ReplyTo, PartsCtx, Fun, Acc0) ->
	{ok, Result} = kyte_parts:fold( PartsCtx, Fun, Acc0 ),
	_Ignored = gen_server:reply(ReplyTo, {ok, Result}),
	ok.

parts_each(ReplyTo, PartsCtx, Fun) ->
	{ok, Result} = kyte_parts:fold( PartsCtx, fun(Partition, _) ->
		Fun(Partition)
	end, ignored ),
	_Ignored = gen_server:reply(ReplyTo, Result),
	ok.

perform_execute(db_count, ReplyTo, PartsCtx) ->
	parts_fold(ReplyTo, PartsCtx, 
		fun(Partition, Acc) ->
			{ok, Count} = gen_server:call(Partition, db_count, infinity),
			Acc + Count
		end, 0);

perform_execute(db_size, ReplyTo, PartsCtx) ->
	parts_fold(ReplyTo, PartsCtx, 
		fun(Partition, Acc) ->
			{ok, Size} = gen_server:call(Partition, db_size, infinity),
			Acc + Size
		end, 0);

perform_execute(db_clear, ReplyTo, PartsCtx) ->
	parts_each(ReplyTo, PartsCtx,
		fun(Partition) ->
			ok = gen_server:call(Partition, db_clear, infinity)
		end);


perform_execute(Op, _ReplyTo, _PartsCtx) ->
	{error, bad_arg, Op}.

