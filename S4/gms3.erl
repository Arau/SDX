-module(gms3).
-export([start/1, start/2]).

start(Id) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Self) end).


init(Id, Master) ->
    leader(Id, Master, 0, []).


start(Id, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Grp, Self) end).


init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, N, State, Leader, _, Peers} ->
            Master ! {ok, State},
            erlang:monitor(process, Leader),             %% Monitoring Master
            slave(Id, Master, Leader, N, '', Peers)      %% No old message

        after 1000 ->
            Master ! {error, "no reply from leader"}
    end.


election(Id, Master, N, Last, [Leader|Rest]) ->                %% Decide who is the new Master
    if                                                 %%  -> First node in List <-
        Leader == self() ->
            leader(Id, Master, N, Rest),               %% Passing Seq. number
            bcast(Id, {msg, N, Last}, Rest),           %% Send last Msg to Peers \
            io:format("I'm the new Leader~n", []);     %%  they discard it, if has already received

        true ->
            erlang:monitor(process, Leader),
            io:format("New Leader: ~w~n", [Leader]),
            slave(Id, Master, Leader, N, Last, Rest)
    end.


leader(Id, Master, N, Peers) -> 
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N+1, Msg}, Peers),     %% Fwd msg to all rest nodes 
            Master ! {deliver, Msg},               %% Reply to worker    
            leader(Id, Master, N+1, Peers);

        {join, Peer} ->
            Master ! request,                      %% Request to change worker color 
            joining(Id, N, Master, Peer, Peers);   %% Add node to peers list

        stop ->
            ok;

        Error ->
            io:format("leader ~w: strange message ~w~n", [Id, Error])
    end.


slave(Id, Master, Leader, N, Last, Peers) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},              %% Fwd msg from worker to leader
            slave(Id, Master, Leader, N+1, Msg, Peers);

        {join, Peer} ->
            Leader ! {join, Peer},              %% Join request to leader
            slave(Id, Master, Leader, N, Last, Peers);

        {msg, I, Msg} when I > N ->                       %% Discard if received msg is older 
                Master ! {deliver, Msg},                  %% Reply to worker
                slave(Id, Master, Leader, I, Msg, Peers);

        {view, N, _, Leader, View} ->
            slave(Id, Master, Leader, N, Last, View);
            
        {'DOWN', _Ref, process, Leader, _Reason} ->   %% Generated by process monitor
            election(Id, Master, N, Last, Peers);     %% when Pid2 has fallen down.

        stop ->
            ok;

        Error ->
            io:format("slave ~w: strange message! ~w~n", [Id, Error])
    end.


joining(Id, N, Master, Peer, Peers) ->
    receive
        {ok, State} ->
            Peers2 = lists:append(Peers, [Peer]),
            bcast(Id, {view, N, State, self(), Peers2}, Peers2),
            leader(Id, N, Master, Peers2);

        stop ->
            ok
    end.


bcast(_, Msg, Nodes) ->
    %% foreach node in Nodes, do Func.
    lists:foreach(
                 fun(Node) -> 
                    Node ! Msg 
                 end, 
                 Nodes).
