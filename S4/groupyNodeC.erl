-module(groupyNodeC).
-export([start/3, stop/0]).

start(Module, Sleep, Leader) ->
    register(c, worker:start("3", Module, 3,{a, Leader}, Sleep)).

stop() ->
    c ! stop.
