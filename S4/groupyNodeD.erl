-module(groupyNodeD).
-export([start/3, stop/0]).

start(Module, Sleep, Leader) ->
    register(d, worker:start("4", Module, 4, {a, Leader}, Sleep)).

stop() ->
    d ! stop.

