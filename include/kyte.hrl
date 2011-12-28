
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


-ifndef(kyte_hrl).
-define(kyte_hrl, included).

-type kyte_value_codec() :: raw | etf | sext | rawz | etfz.

-type hash_fun_term() :: any(). % fun( ( any() ) -> integer() ). %% this one breaks the highlight in Sublime Text 2 :(
-type hash_fun_bin() :: any(). % fun( ( binary() ) -> integer() ).

-type kyte_partitioning_type() ::
	  single 
	| {pre_hash, Count :: integer(), HF :: hash_fun_term() }
	| {post_hash, Count :: integer(), HF :: hash_fun_bin() }.

-record(kyte_db_args, {
	file :: string(),
	
	key_codec = etf :: kyte_value_codec(),
	val_codec = etf :: kyte_value_codec(),

	parts = single :: kyte_partitioning_type()
}).

-type kyte_db_args() :: #kyte_db_args{}.

-endif. % kyte_hrl
