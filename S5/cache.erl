-module(cache).
-export([new/0, lookup/2, add/4, remove/2]).

new() ->
    [].

lookup(Name, Cache) ->
    case lists:keyfind(Name, 1, Cache) of
        {_, Expire, Data} ->
            Now = time:now(),
            if
                Now >= Expire   -> invalid;
                true            -> {ok, Data}
            end;

        false -> unknown
    end.

add(Name, Expire, Data, Cache) ->
    lists:keystore(Name, 1, Cache, {Name, Expire, Reply}).

remove(Name, Cache) ->
    lists:keydelete(Name, 1, Cache).
