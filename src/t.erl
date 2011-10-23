-module(t).
-compile(export_all).

t1() ->
	kps_seq_srv:start_link(),
	kyoto_port_srv:start_link([
		{port_server_binary, "../c_src/kyoto-port-server"}
	]).

t2() ->
	{ok, Kyo} = t1(),
	kyoto_port_srv:open(Kyo, <<"test.kch">>).

t3() ->
	application:start(kyoto_client).