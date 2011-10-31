%% kyte_perf_test:test_db_type("test.kch", 100000), kyte_perf_test:test_db_type("test.kct", 100000), kyte_perf_test:test_db_type("+", 100000), kyte_perf_test:test_db_type("-", 100000).

-module(kyte_perf_test).

-compile(export_all).

test_db_type(F, C) ->
	{ok, PoolID} = kyte:pool_create(2),
	{ok, Db} = kyte:db_open(PoolID, F),
	SeqKeys = lists:seq(1, C),
	T1 = erlang:now(),
	lists:foreach(fun(K) ->
		ok = kyte:db_set(Db, <<K:256/little>>, <<K:512/big>>)
	end, SeqKeys),
	T2 = erlang:now(),
	lists:foreach(fun(K) ->
		{ok, _} = kyte:db_get(Db, <<K:256/little>>)
	end, SeqKeys),
	T3 = erlang:now(),

	RandKeys = random_keys(C),

	T4 = erlang:now(),
	lists:foreach(fun(K) ->
		ok = kyte:db_set(Db, <<K:256/little>>, <<K:512/big>>)
	end, RandKeys),
	T5 = erlang:now(),
	lists:foreach(fun(K) ->
		{ok, _} = kyte:db_get(Db, <<K:256/little>>)
	end, RandKeys),
	T6 = erlang:now(),

	kyte:pool_destroy(PoolID),

	print_report(F, C, [T1, T2, T3, T4, T5, T6]).

random_keys(C) ->
	random_keys(C, sets:new()).

random_keys(0, Set) ->
	sets:to_list(Set);

random_keys(C, Set) ->
	R = random:uniform(100000000),
	case sets:is_element(R, Set) of
		true ->
			random_keys(C, Set);
		false ->
			random_keys(C - 1, sets:add_element(R, Set))
	end.

to_seconds(I, T) -> 
	{Mega, S, Micro} = lists:nth(I, T),
	Mega * 1000000 + S + (Micro / 1000000).

calc_rate(C, T, S, E) ->
	C / ( to_seconds(E, T) - to_seconds(S, T) ).

print_report(F, C, T) ->
	io:format("*** ~p ***~n", [F]),
	io:format("Seq-writes:  ~p ops/s~n", [ calc_rate(C, T, 1, 2) ]),
	io:format("Seq-reads:   ~p ops/s~n", [ calc_rate(C, T, 2, 3) ]),
	io:format("Rand-writes: ~p ops/s~n", [ calc_rate(C, T, 4, 5) ]),
	io:format("Rand-reads:  ~p ops/s~n", [ calc_rate(C, T, 5, 6) ]),
	ok.
	