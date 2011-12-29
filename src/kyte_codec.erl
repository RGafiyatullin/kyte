% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_codec).

-export([
	encode/2,
	decode/2
]).

-include("kyte.hrl").

encode(C, V) ->
	case C of
		raw ->
			V;
		rawz ->
			zlib:zip(V);
		etf ->
			term_to_binary(V);
		etfz ->
			zlib:zip(term_to_binary(V));
		sext ->
			sext:encode(V)
	end.


decode(C, V) ->
	case C of
		raw ->
			V;
		rawx ->
			zlib:unzip(V);
		etf ->
			binary_to_term(V);
		etfz ->
			binary_to_term(zlib:unzip(V));
		sext ->
			sext:decode(V)
	end.
