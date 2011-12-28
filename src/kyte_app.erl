% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

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
	ok.
