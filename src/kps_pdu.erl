-module(kps_pdu).

-export([
	pdu_type/1,
	rec_type/1,

	encode/2,
	decode/2
]).

pdu_type('KPSBasicRequest')			-> 0;
pdu_type('KPSBasicResponse')		-> 1;
pdu_type('KPSSetOptionRequest')		-> 2;
pdu_type('KPSSetOptionResponse')	-> 3;
pdu_type('KPSDbOpenRequest')		-> 4;
pdu_type('KPSDbOpenResponse')		-> 5;
pdu_type('KPSDbCloseRequest')		-> 6;
pdu_type('KPSDbCloseResponse')		-> 7;
pdu_type('KPSDbClearRequest')		-> 8;
pdu_type('KPSDbClearResponse')		-> 9;
pdu_type('KPSDbCountRequest')		-> 10;
pdu_type('KPSDbCountResponse')		-> 11;
pdu_type('KPSDbSizeRequest')		-> 12;
pdu_type('KPSDbSizeResponse')		-> 13;
pdu_type('KPSDbSetRequest')			-> 14;
pdu_type('KPSDbSetResponse')		-> 15;
pdu_type('KPSDbGetRequest')			-> 16;
pdu_type('KPSDbGetResponse')		-> 17;
pdu_type('KPSDbRemoveRequest')		-> 18;
pdu_type('KPSDbRemoveResponse')		-> 19.


rec_type(4) -> 'KPSDbOpenRequest';
rec_type(5) -> 'KPSDbOpenResponse'.

encode(RecType, Record) ->
	{ok, DeepList} = 'KyotoPS':encode(RecType, Record),
	FlatList = lists:flatten(DeepList),
	Bin = list_to_binary(FlatList),
	{ok, Bin}.

decode(RecType, Bin) ->
	{ok, _} = 'KyotoPS':decode(RecType, Bin).
