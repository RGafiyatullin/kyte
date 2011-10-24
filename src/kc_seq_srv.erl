-module(kc_seq_srv).
-behaviour(gen_server).

-define(max_seq_value, 65535).

-export([
	start_link/0,
	next/0
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
	current :: integer()
}).

start_link() ->
	io:format("kc_seq_srv:start_link()~n", []),
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	{ok, #state{ current = 0 }}.

handle_call(get_next, _From, State = #state{ current = Current }) ->
	{reply, Current, State#state{ current = if
												Current == ?max_seq_value -> 0;
												true -> Current + 1
											end }};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% API

next() ->
	gen_server:call(?MODULE, get_next, infinity).

