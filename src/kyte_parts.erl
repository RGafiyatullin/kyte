-module(kyte_parts).

-export([
	file_names/2,
	choose_partition/3,
	with_partition/4
	]).


file_names(DbFile, PartsCount) ->
	lists:map(fun(Idx) ->
		lists:flatten(io_lib:format(DbFile, [Idx]))
	end, lists:seq(1, PartsCount)).

choose_partition(K, HF, Parts) ->
	Kh = HF(K),
	Bits = size(Kh) * 8,
	<<Hash:Bits/unsigned>> = Kh,
	PartIdx = ( Hash rem length(Parts) ) + 1,
	{ok, lists:nth(PartIdx, Parts)}.

with_partition(K, HF, Parts, Func) ->
	{ok, Part} = choose_partition(K, HF, Parts),
	Func(Part).

%%% Internal



