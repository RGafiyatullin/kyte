-ifndef(kyte_hrl).
-define(kyte_hrl, included).

-type kyte_value_codec() :: raw | etf | sext | rawz | etfz.

-record(kyte_db_args, {
	file :: string(),
	key_codec = etf :: kyte_value_codec(),
	val_codec = etf :: kyte_value_codec()
}).
-type kyte_db_args() :: #kyte_db_args{}.

-endif. % kyte_hrl