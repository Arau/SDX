-module(groupyNodeE).
-export([start/3, stop/0]).

start(Module, Sleep, Leader) ->
    register(e, worker:start("5", Module, 5, {a, Leader}, Sleep)).

stop() ->
    e ! stop.

