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

<pre>
{ok, Pool} = kyte:pool_create(8). % Create a pool of eight threads. The pool is linked to the creating process.
</pre>

### Open a Database

Having a pool created you can open a database:

<pre>
DbArgs = #kyte_db_args{
	file = "data/accounts.kch"
},
{ok, Db} = kyte:db_open(Pool, DbArgs). % Open a database. The database is linked to the creating process.
</pre>

### Storing, Getting and Deleting the Values

Storing:

<pre>
Key = {this, is, 'a key'},
Value = ["that", &lt;&lt;"is"&gt;&gt;, ["a", "value"], "!"],
ok = kyte:db_set(Db, Key, Value).
</pre>

Getting:

<pre>
{ok, Value} = kyte:db_get(Db, Key).
</pre>

Deleting:

<pre>
ok = kyte:db_delete(Db, Key).
</pre>

### Close the Database

In order to manually close the database do as follows:

<pre>
ok = kyte:db_close(Db).
</pre>

Actually you do not often have to close your databases manually.
They are linked and will close with the 'owning' process when it's being halted with the reason 'shutdown'.


## How the values are stored

The keys and values are encoded prior to be saved in the database.

Any of the following codecs can be used for keys or values:

*	*raw* - Only raw binaries are expected. Those are stored as they are.

*	*etf* - Any Erlang terms are allowed. They are converted to ETF (with erlang:term_to_binary/1) the product binaries are saved.

*	*sext* - Any Erlang terms are allowed. They are converted to SEXT ([sext:encode/1](https://github.com/uwiger/sext)).
This is completely useless for the Values. 
Though it might be good for encoding the Keys: in future iteration through the collections is planned to be implemented.

*	*rawz* - Same as *raw* but the binaries are zipped prior to be saved (with zlib:zip/1)

*	*etfz* - Same as *etf* but the binaries are zipped prior to be saved.


### To zip or not to zip?

Q: Why do I want to zip anything? Doesn't it consume the CPU.

A: Yes, but it may save the IO.


Q: When should I rather not zip?

A: When the zip product is bigger than the source. E.g. short pieces of data: integers, UUIDs, logins - most of the keys' types.


## API

<pre>
-spec pool_create( PoolSize :: integer() ) -> {ok, Pool :: pid()} | {error, any()}.
</pre>

Create and link to the calling process a pool of the given size.

<pre>
-spec pool_destroy( Pool :: pid() ) -> ok | {error, any()}.
</pre>

Destroy the pool. The pool will terminate with 'normal' reason. The affiliated databases will terminate with the 'rudely_closed' reason.

<pre>
-spec db_open( Pool :: pid(), kyte_db_args() ) -> {ok, DbSrv :: pid() }.
</pre>

Open a database. See #kyte_db_args for details.


<pre>
-spec db_close( DbSrv :: pid() ) -> ok.
</pre>

Close the database. It will terminate with 'normal' reason.


<pre>
-spec db_set( DbSrv :: pid(), Key :: term(), Value :: term() ) -> ok | {error, any()}.
</pre>

Store Key-Value pair in the database DbSrv.


<pre>
-spec db_get( DbSrv :: pid(), Key :: term() ) -> {ok, Value :: term()} | {error, any()}.
</pre>

This function searches for a Key in the database.


<pre>
-spec db_del( DbSrv :: pid(), Key :: term() ) -> ok | {error, any()}.
</pre>

Removes the Key from the database.


<pre>
-spec db_count( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
</pre>

Gets rows' count in the database.


<pre>
-spec db_size( DbSrv :: pid() ) -> {ok, integer()} | {error, any()}.
</pre>

Returns the size of the database in bytes.


<pre>
-spec db_clear( DbSrv :: pid() ) -> ok | {error, any()}.
</pre>

Clears the database.


<pre>
-type hash_fun_bin() :: fun( ( binary() ) -> integer() ).
-type kyte_partitioning_type() ::
	  single 
	| {post_hash, Count :: integer(), HashF :: hash_fun_bin() }.
-record(kyte_db_args, {
	file :: string(),
	key_codec = etf :: kyte_value_codec(),
	val_codec = etf :: kyte_value_codec(),
	parts = single :: kyte_partitioning_type()
}).
</pre>

* file - path to the database. See [PolyDB::open(const std::string&, uint32_t)](http://fallabs.com/kyotocabinet/api/classkyotocabinet_1_1PolyDB.html#a09384a72e6a72a0be98c80a1856f34aa) for the filenames' meaning.

* key_codec, val_codec - the codecs to be used with keys and values respectively.

* parts - partitioning to be used. Can be either a single partition or a key-hash based partitioning.

