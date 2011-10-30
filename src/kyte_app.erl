-module(kyte_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_KYTE_THR_POOL_SIZE, 16).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ThrPoolSize = ?DEFAULT_KYTE_THR_POOL_SIZE,
	kyte_nifs:create_thr_pool(ThrPoolSize),
	kyte_sup:start_link().

stop(_State) ->
	kyte_nifs:destroy_thr_pool(0),
    ok.
