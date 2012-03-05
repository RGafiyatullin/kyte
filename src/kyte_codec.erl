% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

-module(kyte_codec).

-export([
	encode/2,
	decode/2
]).

-include("kyte.hrl").

encode(raw, V) -> V;
encode(rawz, V) -> zlib:zip(V);
encode(etf, V) -> term_to_binary(V);
encode(etfz, V) -> zlib:zip(term_to_binary(V));
encode(sext, V) -> sext:encode(V);
encode(Other, _V) -> error({bad_codec, Other}).

decode(raw, V) -> V;
decode(rawz, V) -> zlib:unzip(V);
decode(etf, V) -> binary_to_term(V);
decode(etfz, V) -> binary_to_term(zlib:unzip(V));
decode(sext, V) -> sext:decode(V);
decode(Other, _V) -> error({bad_codec, Other}).
