-module(kyte).

-export([start/0, stop/0]).

start() -> application:start(kyte).
stop() -> application:stop(kyte).



