% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_srv).

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

-include("kyte.hrl").

-record(state, {}).

start_link(Pool, Args) ->
	gen_server:start_link(?MODULE, {Pool, Args}, []).

init({{Pool, #kyte_db_args{
	file = DbFile,
	parts = DbPartsType
}}) ->
	Parts = kyte_parts:init(Pool, DbFile, DbPartsType),
	{ok, #state{}}.

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


