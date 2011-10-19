-module(t).
-compile(export_all).

t1() ->
	kyoto_port_srv:start_link([
		{db_file, <<"./test.kch">>}
	]).
