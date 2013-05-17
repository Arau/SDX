-module(entry).
-export([lookup/2, add/3, remove/2]).

lookup(Req, Entries) ->
    Tuple = lists:keyfind(Req, 1, Entries),
    if Tuple == false -> unknown;
                true  -> Tuple
    end.

add(Name, Entry, Entries) ->
    lists:keystore(Name, 1, Entries, {Name, Entry}).

remove(Name, Entries) ->
    lists:keydelete(Name, 1, Entries).
