% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_db_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-include("kyte.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Pool, DbArgs = #kyte_db_args{}) ->
    { ok, Sup } = supervisor:start_link( ?MODULE, { Pool, DbArgs } ),
    { ok, PartsSup } = supervisor:start_child( Sup, spec_parts_sup() ),
    { ok, DbSrv } = supervisor:start_child( Sup, spec_db_srv(Pool, PartsSup, DbArgs) ),
    { ok, Sup, DbSrv }.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({Pool, DbArgs}) ->
    {ok, { {one_for_all, 5, 10}, []} }.

spec_parts_sup() ->
	{parts_sup, {kyte_db_partition_sup, start_link, []}, permanent, infinity, supervisor, [kyte_db_partition_sup]}.

spec_db_srv(Pool, PartsSup, DbArgs) ->
	{db_srv, {kyte_db_srv, start_link, [Pool, PartsSup, DbArgs]}, permanent, infinity, supervisor, [kyte_db_srv]}.