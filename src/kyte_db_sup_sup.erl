% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_sup_sup).
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
    { ok, _Sup } = supervisor:start_link( {local, ?MODULE}, ?MODULE, {} ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({}) ->
    {ok, { {simple_one_for_one, 5, 10}, [
    	{db_sup, {kyte_db_sup, start_link, []}, temporary, infinity, supervisor, [kyte_db_sup]}
    ]} }.
