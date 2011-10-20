-module(t).
-compile(export_all).

t1() ->
	kps_seq_srv:start_link(),
	kyoto_port_srv:start_link([
		{port_server_binary, "../c_src/kyoto-port-server"}
	]).
