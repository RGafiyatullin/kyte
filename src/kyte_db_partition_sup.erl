% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_partition_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("kyte.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    { ok, _Sup } = supervisor:start_link( ?MODULE, {} ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({}) ->
    {ok, { {one_for_one, 5, 10}, []} }.

