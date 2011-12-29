% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_request_srv).

-behaviour(gen_server).

-export([
	start_link/4,
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

-include("kyte.hrl").

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
	parts_ctx :: term(),
	codecs :: {kyte_value_codec(), kyte_value_codec()}
}).

start_link(PartsCtx, Codecs, Operation, ReplyTo) ->
	gen_server:start_link(?MODULE, {PartsCtx, Codecs, Operation, ReplyTo}, []).

init({PartsCtx, Codecs, Operation, ReplyTo}) ->
	{ok, #state{
		operation = Operation,
		reply_to = ReplyTo,
		parts_ctx = PartsCtx,
		codecs = Codecs
	}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(execute, State = #state{
	operation = Op,
	reply_to = ReplyTo,
	parts_ctx = PartsCtx,
	codecs = Codecs
}) ->
	{LinkTo, _} = ReplyTo,
	erlang:link(LinkTo),
	ok = perform_execute(Op, ReplyTo, PartsCtx, Codecs),
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
	Result = kyte_parts:fold( PartsCtx, Fun, Acc0 ),
	_Ignored = gen_server:reply(ReplyTo, Result),
	ok.

perform_execute(db_count, ReplyTo, PartsCtx, _Codecs) ->
	parts_fold(ReplyTo, PartsCtx, 
		fun(Partition, {ok, Acc}) ->
			{ok, Count} = gen_server:call(Partition, db_count, infinity),
			{ok, Acc + Count}
		end, {ok,0});

perform_execute(db_size, ReplyTo, PartsCtx, _Codecs) ->
	parts_fold(ReplyTo, PartsCtx, 
		fun(Partition, {ok,Acc}) ->
			{ok, Size} = gen_server:call(Partition, db_size, infinity),
			{ok, Acc + Size}
		end, {ok,0});

perform_execute(db_clear, ReplyTo, PartsCtx, _Codecs) ->
	parts_fold(ReplyTo, PartsCtx,
		fun(Partition, ok) ->
			ok = gen_server:call(Partition, db_clear, infinity)
		end, ok);

perform_execute({db_set, K, V}, ReplyTo, PartsCtx, {KCodec, VCodec}) ->
	Kenc = kyte_codec:encode(KCodec, K),
	Venc = kyte_codec:encode(VCodec, V),
	Partition = kyte_parts:choose_partition(PartsCtx, K, Kenc),
	Ret = gen_server:call(Partition, {db_set, Kenc, Venc}, infinity),
	_Ignored = gen_server:reply(ReplyTo, Ret),
	ok;

perform_execute({db_get, K}, ReplyTo, PartsCtx, {KCodec, VCodec}) ->
	Kenc = kyte_codec:encode(KCodec, K),
	Partition = kyte_parts:choose_partition(PartsCtx, K, Kenc),
	ReplyWith = case gen_server:call(Partition, {db_get, Kenc}, infinity) of
		{ok, Venc} ->
			{ok, kyte_codec:decode(VCodec, Venc)};
		AnythingElse ->
			AnythingElse
	end,
	_Ignored = gen_server:reply(ReplyTo, ReplyWith),
	ok;

perform_execute({db_del, K}, ReplyTo, PartsCtx, {KCodec, _VCodec}) ->
	Kenc = kyte_codec:encode(KCodec, K),
	Partition = kyte_parts:choose_partition(PartsCtx, K, Kenc),
	ReplyWith = gen_server:call(Partition, {db_remove, Kenc}, infinity),
	_Ignored = gen_server:reply(ReplyTo, ReplyWith),
	ok;

perform_execute(Op, _ReplyTo, _PartsCtx, _Codecs) ->
	{error, bad_arg, Op}.

