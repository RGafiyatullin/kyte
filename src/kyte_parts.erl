% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_parts).

% -export([
% 	file_names/2,
% 	choose_partition/3,
% 	with_partition/4
% 	]).

-export([
	init/5,
	partition_init_notify/3,
	close_partitions/1
]).

-include("kyte.hrl").

-record(state, {
	type :: kyte_partitioning_type(),
	partitions :: dict()
}).

-type state() :: #state{}.
-type part_id() :: single | {part, integer()}.

-spec init(pid(), pid(), pid(), string(), kyte_partitioning_type()) -> state().
-spec partition_init_notify( state(), part_id(), pid() ) -> state().
-spec close_partitions(state()) -> ok.

init( Pool, DbSrv, PartsSup, DbFile, single) ->
	{ok, SinglePartSpec} = spec_single_partition(single, Pool, DbSrv, DbFile),
	{ok, SinglePartSrv} = supervisor:start_child(PartsSup, SinglePartSpec),
	Dict = dict:store(single, SinglePartSrv, dict:new()),
	#state{
		type = single,
		partitions = dict:store(single, SinglePartSrv, Dict )
	};

init( Pool, DbSrv, PartsSup, DbFile, Type = {post_hash, PC, _HF}) ->
	{ok, Dict} = start_multiple_partitions(dict:new(), PC, Pool, DbSrv, PartsSup, DbFile),
	#state{
		type = Type,
		partitions = Dict
	}.

partition_init_notify( PartsCtx = #state{ type = single }, single, PartSrv ) ->
	{ ok, PartsCtx #state{
		partitions = dict:store( single, PartSrv, dict:new() )
	} };

partition_init_notify( PartsCtx = #state{ 
							type = {post_hash, _PC, _HF},
							partitions = Dict
						}, ID, PartSrv
) ->
	{ ok, PartsCtx #state{
		partitions = dict:store( ID, PartSrv, Dict )
	} }.

close_partitions(#state{
	partitions = Dict
}) ->
	lists:foreach( fun({_, PartSrv}) ->
		io:format("Shutting down ~p~n", [PartSrv]),
		Ret = gen_server:call(PartSrv, db_close, infinity),
		io:format("Shutting down ~p -> ~p~n", [PartSrv, Ret])
	end, dict:to_list(Dict) ),
	ok.






%%% Internal

start_multiple_partitions( Dict, 0, _Pool, _DbSrv, _PartsSup, _DbFile ) ->
	{ok, Dict};

start_multiple_partitions( Dict, PC, Pool, DbSrv, PartsSup, DbFile ) ->
	{ ok, PartSpec } = spec_single_partition( { part, PC }, Pool, DbSrv, file_name( DbFile, PC ) ),
	{ ok, PartSrv } = supervisor:start_child( PartsSup, PartSpec ),
	start_multiple_partitions(
		dict:store({part, PC}, PartSrv, Dict),
		PC - 1, Pool, DbSrv, PartsSup, DbFile
	).

spec_single_partition( ID, Pool, DbSrv, DbFile ) ->
	{ ok, 
		{ ID, 
			{ kyte_db_partition_srv, start_link, [ ID, Pool, DbSrv, DbFile ] }, 
			transient, 30000, worker, [ kyte_db_partition_srv ] 
		}
	}.

file_name(DbFile, Part ) ->
	lists:flatten( io_lib:format( DbFile, [ Part ] ) ).




% file_names(DbFile, PartsCount) ->
% 	lists:map(fun(Idx) ->
% 		lists:flatten(io_lib:format(DbFile, [Idx]))
% 	end, lists:seq(1, PartsCount)).

% choose_partition(K, HF, Parts) ->
% 	Kh = HF(K),
% 	Bits = size(Kh) * 8,
% 	<<Hash:Bits/unsigned>> = Kh,
% 	PartIdx = ( Hash rem length(Parts) ) + 1,
% 	{ok, lists:nth(PartIdx, Parts)}.

% with_partition(K, HF, Parts, Func) ->
% 	{ok, Part} = choose_partition(K, HF, Parts),
% 	Func(Part).

%%% Internal



