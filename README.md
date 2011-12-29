# Kyte - a Binding from Erlang to Kyoto Cabinet.

## Trivia

[Kyoto Cabinet](http://fallabs.com/kyotocabinet/) is an embeddable key-value storage.

Kyte uses NIFs to bind to it. 
Unpredictably long 'unmanaged' requests to the database are executed in the therad pool. 
This lets not to bother the BEAM scheduler.

## Usage

### Create a Pool

Work with any Kyoto database is performed inside a thread pool.
You can create several thread pools to work with different databases relatively independently.

<code>
{ok, Pool} = kyte:pool_create(8). % Create a pool of eight threads. The pool is linked to the creating process.
</code>

### Open a Database

Having a pool created you can open a database:

<code>
DbArgs = #kyte_db_args{
	file = "data/accounts.kch"
},
{ok, Db} = kyte:db_open(Pool, DbArgs). % Open a database. The database is linked to the creating process.
</code>

### Storing, Getting and Deleting the Values

Storing:

<code>
Key = {this, is, 'a key'},
Value = ["that", <<"is">>, ["a", "value"], "!"],
ok = kyte:db_set(Db, Key, Value).
</code>

Getting:

<code>
{ok, Value} = kyte:db_get(Db, Key).
</code>

Deleting:

<code>
ok = kyte:db_delete(Db, Key).
</code>

### Close the Database

In order to manually close the database do as follows:

<code>
ok = kyte:db_close(Db).
</code>

Actually you do not often have to close your databases manually.
They are linked and will close with the 'owning' process when it's being halted with the reason 'shutdown'.


## How the values are stored

The keys and values are encoded prior to be saved in the database.

Any of the following codecs can be used for keys or values:

*	*raw* 
Only raw binaries are expected. Those are stored as they are.

*	*etf* 
Any Erlang terms are allowed. They are converted to ETF (with erlang:term_to_binary/1) the product binaries are saved.

*	*sext* 
Any Erlang terms are allowed. They are converted to SEXT (with [sext:encode/1](https://github.com/uwiger/sext))

This is completely useless for the Values. 
Though it might be good for encoding the Keys: in future iteration through the collections is planned to be implemented.

*	*rawz* 
Same as *raw* but the binaries are zipped prior to be saved (with zlib:zip/1)

*	*etfz*
Same as *etf* but the binaries are zipped prior to be saved.


## API

<code>
-spec pool_create( PoolSize :: integer() ) -> {ok, Pool :: pid()} | {error, any()}.
</code>

<code>
-spec pool_destroy( Pool :: pid() ) -> ok | {error, any()}.
</code>

<code>
-spec db_open( Pool :: pid(), kyte_db_args() ) -> {ok, DbSrv :: pid() }.
</code>

<code>
-spec db_close( DbSrv :: pid() ) -> ok.
</code>

<code>
-spec db_set( DbSrv :: pid(), K :: term(), V :: term() ) -> ok | {error, any()}.
</code>

<code>
-spec db_get( DbSrv :: pid(), K :: term() ) -> {ok, Value :: term()} | {error, any()}.
</code>

<code>
-spec db_del( DbSrv :: pid(), K :: term() ) -> ok | {error, any()}.
</code>

<code>
-spec db_count( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
</code>

<code>
-spec db_size( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
</code>

<code>
-spec db_clear( DbSrv :: pid() ) -> ok | {error, any()}.
</code>



