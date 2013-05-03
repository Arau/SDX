-module(groupyNodeB).
-export([start/3, stop/0]).

start(Module, Sleep, Leader) ->
    register(b, worker:start("2", Module, 2, {a, Leader}, Sleep)).

stop() ->
    b ! stop.

