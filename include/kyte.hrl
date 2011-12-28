% This file is a part of Kyte released under the MIT licence.
% See the LICENCE file for more information

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
