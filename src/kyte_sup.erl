% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
    	{pool_sup, {kyte_pool_sup, start_link, []}, permanent, infinity, supervisor, [kyte_pool_sup]}
    ]} }.

