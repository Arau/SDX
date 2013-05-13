-module(entry).
-export([lookup/2, add/3, remove/2]).

lookup(Req, Entries) ->
    Tuple = lists:keyfind(Req, 1, Entries),
    if Tuple == false -> unknown;
                true ->  Tuple
    end.

%%add(Name, Entry, Entries) ->

%%remove(Name, Entries) ->
