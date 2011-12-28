
% Copyright (C) 2011 Roman Gafiyatullin <romko.goofique@gmail.com>

% Permission is hereby granted, free of charge, to any person obtaining a copy of
% this software and associated documentation files (the "Software"), to deal in
% the Software without restriction, including without limitation the rights to
% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is furnished to do
% so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

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



