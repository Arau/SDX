-module(groupyNode).
-export([start/5, stop/0]).

start(Module, Sleep, Leader, Node, NumNode) ->
    register(Node, worker:start(integer_to_list(NumNode), Module, NumNode, {a, Leader}, Sleep)).

stop() ->
    Node ! stop.

