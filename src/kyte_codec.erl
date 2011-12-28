
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
