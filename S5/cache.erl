-module(cache).
-export([new/0, lookup/2, add/4, remove/2]).

new() ->
    [].

lookup(Name, Cache) ->
    Result = lists:keyfind(Name, 1, Cache),
    if
        Result == false         -> unknown;
        element(1, Result) == 0 -> invalid;
        true                    -> Result
    end.

add(Fqdn, Expire, Data, Cache) ->
    NewCache = [{Fqdn, Expire, Data} | Cache],
    NewCache.

remove(Name, Cache) ->
    lists:keydelete(Name, 1, Cache).
