-module(lock3).

-export([init/2]).

init(Id, Nodes) ->
    Clock = 0,
    open(Nodes, Id, Clock).

open(Nodes, Id, Clock) ->
    receive
        {take, Master} ->
            Refs = requests(Nodes, Id, Clock),
            wait(Nodes, Master, Refs, [], Id, Clock, Clock);

        {request, From, Ref, _, Rclock} ->
            Newclock = list:max([Clock,Rclock]) + 1,
            From ! {ok, Ref, Newclock},
            open(Nodes, Id, Newclock);

        stop ->
            ok

    end.

requests(Nodes, Id, Clock) ->
    lists:map(
        fun(P) ->
            R = make_ref(),
            P ! {request, self(), R, Id, Clock},
            R
        end,
        Nodes).

wait(Nodes, Master, [], Waiting, Id, Clock, _) ->
    Master ! taken,
    held(Nodes, Waiting, Id, Clock);

wait(Nodes, Master, Refs, Waiting, Id, Clock, Rclock) ->
    receive
        {request, From, Ref, Rid, Rclock2} ->
            Newclock = lists:max([Clock, Rclock2]) + 1,
            if
                Rclock > Rclock2 ->
                    From ! {ok, Ref, Newclock},
                    wait(Nodes, Master, Refs, Waiting, Id, Newclock, Rclock);

                Rclock == Rclock2 ->
                    if
                        Id > Rid ->
                            From ! {ok, Ref, Newclock},
                            wait(Nodes, Master, Refs, Waiting, Id, Newclock, Rclock);

                        true -> 
                            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], Id, Newclock, Rclock)
                    end;

                true ->
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], Id, Newclock, Rclock)
            end;


        {ok, Ref, Rclock3} ->
            Newclock2 = lists:max([Clock, Rclock3]) + 1,
            Refs2 = lists:delete(Ref, Refs),
            wait(Nodes, Master, Refs2, Waiting, Id, Newclock2, Rclock);

        release ->
            ok(Waiting, Clock),
            open(Nodes, Id, Clock)
    end.

ok(Waiting, Clock) ->
    lists:map(
        fun({F,R}) ->
            F ! {ok, R, Clock}
    end,
    Waiting).

held(Nodes, Waiting, Id, Clock) ->
    receive
        {request, From, Ref, _, Rclock} ->
            Newclock = lists:max([Clock, Rclock]) + 1,
            held(Nodes, [{From, Ref}|Waiting], Id, Newclock);

        release ->
            ok(Waiting, Clock),
            open(Nodes, Id, Clock)
    end.

