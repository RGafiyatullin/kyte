% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_request_sup).

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
    supervisor:start_link(?MODULE, {}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({}) ->
    {ok, { {simple_one_for_one, 5, 10}, [
    	{pool, {kyte_db_request_srv, start_link, []}, temporary, 50000, worker, [kyte_db_request_srv]}
    ]} }.
