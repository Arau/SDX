-module(groupy).
-export([start/3, stop/0]).

start(Module, Sleep, Leader) ->
    register(c, worker:start("3", Module, 3, Leader, Sleep)).

stop() ->
    c ! stop.
