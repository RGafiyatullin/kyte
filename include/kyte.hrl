-ifndef(kyte_hrl).
-define(kyte_hrl, included).

-type kyte_value_codec() :: raw | etf | sext | rawz | etfz.

-type part_fun_term() :: any(). % fun( ( any() ) -> integer() ). %% this one breaks the highlight in Sublime Text 2 :(
-type part_fun_bin() :: any(). % fun( ( binary() ) -> integer() ).

-type kyte_partitioning_policy() ::
	  single 
	| {pre_codec_func, F :: part_fun_term() }
	| {post_codec_fun, F :: part_fun_bin() }.

-record(kyte_db_args, {
	file :: string(),
	
	key_codec = etf :: kyte_value_codec(),
	val_codec = etf :: kyte_value_codec(),

	parts = single :: kyte_partitioning_policy()
}).

-type kyte_db_args() :: #kyte_db_args{}.

-endif. % kyte_hrl
