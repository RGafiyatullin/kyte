Kyte - a binding from Erlang to Kyoto Cabinet.
=============================================

Trivia
------
[Kyoto Cabinet](http://fallabs.com/kyotocabinet/) is an embeddable key-value storage.
Kyte uses NIFs to bind to it. 
Unpredictably long 'unmanaged' requests to the database are executed in the therad pool. This lets not to bother the BEAM scheduler.


