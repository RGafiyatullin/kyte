-module(kyte_codec).

-export([
	encode_key/2,

	encode_value/2,
	decode_value/2
]).

-include("kyte.hrl").

encode_key(K, #kyte_db_args{
	key_codec = C
}) ->
	encode_with(C, K).

encode_value(V, #kyte_db_args{
	val_codec = C
}) ->
	encode_with(C, V).


decode_value(V, #kyte_db_args{
	val_codec = C
}) ->
	decode_with(C, V).


%%% Internal

encode_with(C, V) ->
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


decode_with(C, V) ->
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
