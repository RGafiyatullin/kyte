-module(t).
-compile(export_all).

t1() ->
	kc_seq_srv:start_link(),
	kyoto_port_srv:start_link([
		{port_server_binary, "../c_src/kyoto-port-server"},
		{debug_log_file, "test.log"}
	]).

t2() ->
	kc_seq_srv:start_link(),
	{ok, Kyo} = t1(),
	kyoto_port_srv:open(Kyo, <<"test.kch">>).

t3() ->
	kyoto:start(),
	{ok, Srv} = kyoto:start_server([
		{port_server_binary, "../c_src/kyoto-port-server"},
		{debug_log_file, "./test.log"}
	]),
	Handle = kyoto:open(Srv, "test.kch").

t4() ->
	{ok, Db} = t3(),
	kyoto:close(Db).


gen_kv_set(Size) ->
	generate_kv_set_internal(Size, 0, []).

generate_kv_set_internal(0, V, Acc) ->
	Acc;
generate_kv_set_internal(K, V, Acc) ->
	Rand = random:uniform(1000000000000),
	Tup = { <<Rand:128/little>>, <<V:1024/little>>},
	generate_kv_set_internal(K - 1, V + 1, [Tup | Acc]).

perform_generate_and_insert(Size, Fun) ->
	io:format("~p Generating test set.~n", [calendar:local_time()]),
	Set = gen_kv_set(Size),
	io:format("~p Test set is ready. Proceeding with single-threaded test.~n", [calendar:local_time()]),
	Fun(Set),
	io:format("~p Test complete.~n", [calendar:local_time()]).

insert_one_by_one([], _) ->
	ok;
insert_one_by_one([{K, V} | SoFar], Db) ->
	kyoto:set(Db, K, V),
	insert_one_by_one(SoFar, Db).


insert_single_thraded(Db, Size) ->
	%{ok, Db} = t3(),
	io:format("Count: ~p~n", [kyoto:count(Db)]),
	perform_generate_and_insert(Size, fun(Set) ->
		insert_one_by_one(Set, Db)
	end),
	io:format("Count: ~p~n", [kyoto:count(Db)]).




