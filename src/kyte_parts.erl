% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_parts).

% -export([
% 	file_names/2,
% 	choose_partition/3,
% 	with_partition/4
% 	]).

-export([
	init/3
]).

-record(state, {
	type :: kyte_partitioning_type(),
	partitions :: [pid()]
}).

init(Pool, DbFile, single) ->
	{ok, SinglePart} = start_single_partition(Pool, DbFile),
	#state{
		type = single,
		partitions = SinglePart
	};

init(Pool, DbFile, Type = {post_hash, PC, HF}) ->
	{ok, PartsList} = start_multiple_parts(Pool, DbFile, PC),
	#state{
		type = Type,
		partitions = PartsList
	}.


start_single_partition(Pool, DbFile) ->
	{error, not_impl}.

start_multiple_parts(Pool, DbFile, PC) ->
	{error, not_impl}.


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



