-module(lock2).

-export([init/2]).

init(Id, Nodes) ->
    open(Nodes, Id).

open(Nodes, Id) ->
    receive
        {take, Master} ->
            Refs = requests(Nodes, Id),
            wait(Nodes, Master, Refs, [], Id);

        {request, From, Ref} ->
            From ! {ok, Ref},
            open(Nodes, Id);

        stop ->
            ok

    end.

requests(Nodes, Id) ->
    lists:map(
        fun(P) ->
            R = make_ref(),
            P ! {request, self(), R, Id},
            R
        end,
        Nodes).

wait(Nodes, Master, [], Waiting, Id) ->
    Master ! taken,
    held(Nodes, Waiting, Id);

wait(Nodes, Master, Refs, Waiting, Id) ->
    receive
        {request, From, Ref, Rid} ->
            if Id > Rid ->
                From ! {ok, Ref},
                Ref2 = requests([From], Id),
                wait(Nodes, Master, Ref2, Waiting, Id);

                true -> 
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], Id)
            end;

        {ok, Ref} ->
            Refs2 = lists:delete(Ref, Refs),
            wait(Nodes, Master, Refs2, Waiting, Id);

        release ->
            ok(Waiting),
            open(Nodes, Id)
    end.

ok(Waiting) ->
    lists:map(
        fun({F,R}) ->
            F ! {ok, R}
    end,
    Waiting).

held(Nodes, Waiting, Id) ->
    receive
        {request, From, Ref, _} ->
            held(Nodes, [{From, Ref}|Waiting], Id);

        release ->
            ok(Waiting),
            open(Nodes, Id)
    end.

