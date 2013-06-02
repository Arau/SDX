-module(node1).
-export([node/3, start/1, start/2]).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),
    node(MyKey, Predecessor, Successor).

-define(Timeout, 5000).

connect(MyKey, nil) ->
    {ok, {MyKey, self()}};                                  %% I'm the only node in ring

connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, PeerPid}}                           %% Succesor replies

    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])

    end.

-define(Stabilize, 1000).

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, MyKey, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify,{MyKey, self()}},                 %% Send notify msg to my new succesor
            Successor;
        {MyKey, _} ->
            Successor;
        {Skey, _} ->
            Spid ! {notify, {MyKey, self()}},                %% Send notify msg to my new succesor
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    Xpid ! {request, self()},                %% Pred. is my new Successor
                    Pred;                                    %% Stabilize my own
                false ->
                    Spid ! {notify, {MyKey, self()}},        %% Send notify msg to my new succesor
                    Successor
            end
    end.

node(MyKey, Predecessor, Successor) ->
    receive
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor);

        {notify, New} ->
            Pred = notify(New, MyKey, Predecessor),
            node(MyKey, Pred, Successor);

        {request, Peer} ->
            request(Peer, Predecessor),
            node(MyKey, Predecessor, Successor);

        {status, Pred} ->
            Succ = stabilize(Pred, MyKey, Successor),
            node(MyKey, Predecessor, Succ);

        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor);

        probe ->
            create_probe(MyKey, Successor),
            node(MyKey, Predecessor, Successor);

        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor);

        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey | Nodes], T, Successor),
            node(MyKey, Predecessor, Successor)
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.


notify({Nkey, Npid}, MyKey, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};                           %% I'm new predecessor

        {Pkey, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    {Nkey, Npid};                   %% I'm new predecessor
                false ->
                    Predecessor
        end
    end.

create_probe(MyKey, {_, Spid}) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w!~n", [MyKey]).

remove_probe(MyKey, Nodes, T) ->
    Time = timer:now_diff(erlang:now(), T),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(RefKey, Nodes, T, {_, Spid}) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]).

