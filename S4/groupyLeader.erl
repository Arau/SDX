-module(groupyLeader).
-export([start/2, stop/0]).

start(Module, Sleep) ->
    Leader = worker:start("1", Module, 1, Sleep),
    register(a, Leader).

stop() ->
    a ! stop.

