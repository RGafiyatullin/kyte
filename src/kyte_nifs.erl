
% Copyright (C) 2011 Roman Gafiyatullin <romko.goofique@gmail.com>

% Permission is hereby granted, free of charge, to any person obtaining a copy of
% this software and associated documentation files (the "Software"), to deal in
% the Software without restriction, including without limitation the rights to
% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is furnished to do
% so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.


-module(kyte_nifs).

-on_load(load_nif/0).

-export([
	execute_sync/1,
	execute_async/1
]).
-export([
	create_thr_pool/1,
	destroy_thr_pool/1,

	db_open/4,
	db_close/4,

	db_set/6,
	db_get/5,
	db_remove/5,

	db_clear/4,
	db_count/4,
	db_size/4
]).

load_nif() ->
	PrivD = code:priv_dir(kyte),
	NifF = "kyte_nifs",
	NifPath = filename:join(PrivD, NifF),
	ok = erlang:load_nif(NifPath, 0),
	
	init_nif(),
	ok.

-spec init_nif() -> ok | error | any().
init_nif() -> {error, nif_not_loaded}.


-spec db_open(ReplyPid :: pid(), ReplyRef :: reference(),
				PoolIdx :: integer(), PathToDb :: string() ) ->
	{ok, DbIdx :: integer()} |
	{error, any()}.
db_open(_,_,_,_) -> {error, nif_not_loaded}.

-spec db_close(ReplyPid :: pid(), ReplyRef :: reference(),
				PoolIdx :: integer(), DbIdx :: integer() ) -> ok | {error, any()}.
db_close(_,_,_,_) -> {error, nif_not_loaded}.

-spec create_thr_pool(Size :: integer()) -> {ok, integer()} | {error, any()}.
create_thr_pool(_) -> {error, nif_not_loaded}.

-spec destroy_thr_pool(PoolIdx :: integer()) -> ok | {error, any()}.
destroy_thr_pool(_) -> {error, nif_not_loaded}.

-spec db_set(
			ReplyPid :: pid(), ReplyRef :: reference(),
			PoolIdx :: integer(), DbIdx :: integer(), 
			Key :: binary(), Value :: binary()
		) -> ok | {error, any()}.
db_set(_,_,_,_,_,_) ->
	{error, nif_not_loaded}.

-spec db_get(
		ReplyPid :: pid(), ReplyRef :: reference(),
		PoolIdx :: integer(), DbIdx :: integer(),
		Key :: binary()
	) -> ok | {error, any()}.
db_get(_,_,_,_,_) ->
	{error, nif_not_loaded}.

-spec db_remove(
		ReplyPid :: pid(), ReplyRef :: reference(),
		PoolIdx :: integer(), DbIdx :: integer(),
		Key :: binary()
	) -> ok | {error, any()}.
db_remove(_,_,_,_,_) ->
	{error, nif_not_loaded}.

-spec db_clear(
		ReplyPid :: pid(), ReplyRef :: reference(),
		PoolIdx :: integer(), DbIdx :: integer()
	) -> ok | {error, any()}.
db_clear(_,_,_,_) -> {error, nif_not_loaded}.

-spec db_count(
		ReplyPid :: pid(), ReplyRef :: reference(),
		PoolIdx :: integer(), DbIdx :: integer()
	) -> {ok, integer()} | {error, any()}.
db_count(_,_,_,_) -> {error, nif_not_loaded}.

-spec db_size(
		ReplyPid :: pid(), ReplyRef :: reference(),
		PoolIdx :: integer(), DbIdx :: integer()
	) -> {ok, integer()} | {error, any()}.
db_size(_,_,_,_) -> {error, nif_not_loaded}.

execute_sync(Fun) ->
	Ref = make_ref(),
	case Fun(Ref) of
		ok ->	
			receive
				{Ref, AsyncReply} ->
					AsyncReply
			end;
		SyncReply ->
			SyncReply
	end.

execute_async(Fun) ->
	Ref = make_ref(),
	case Fun(Ref) of
		ok ->
			{async, fun() ->
				receive
					{Ref, AsyncReply} ->
						AsyncReply
				end
			end};
		SyncReply ->
			{sync, SyncReply}
	end.

