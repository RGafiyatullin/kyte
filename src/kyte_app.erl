-module(kyte_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	kyte_sup:start_link().

stop(_State) ->
	io:format("kyte_app:stop(_)~n", []),
	ok.
