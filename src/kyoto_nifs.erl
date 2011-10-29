-module(kyoto_nifs).

-on_load(load_nif/0).

-export([
	create_thr_pool/1,
	db_open/4
]).

load_nif() ->
	PrivD = code:priv_dir(kyoto_client),
	NifF = "kyoto_nifs",
	NifPath = filename:join(PrivD, NifF),
	io:format("loading ~p~n", [NifPath]),
	Result = erlang:load_nif(NifPath, 0),
	io:format("load_nif result: ~p~n", [Result]),

	init_nif(),
	ok.

-spec init_nif() -> ok | error | any().
init_nif() -> {error, nif_not_loaded}.


-spec db_open(PoolIdx :: integer(), PathToDb :: string(), ReplyPid :: pid(), ReplyRef :: reference() ) ->
	{ok, DbIdx :: integer()} |
	{error, any()}.

db_open(_PoolIdx, _PathToDb, _ReplyPid, _ReplyRef) -> {error, nif_not_loaded}.


-spec create_thr_pool(Size :: integer()) -> {ok, any()} | {error, any()}.
create_thr_pool(_Size) -> {error, nif_not_loaded}.

