-module(kyoto_nifs).

-on_load(load_nif/0).

load_nif() ->
	erlang:load_nif("priv/kyoto_nifs.so", 0).
