-module(node2).
-export([node/4, start/1, start/2]).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),
    node(MyKey, Predecessor, Successor, []).

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

node(MyKey, Predecessor, Successor, Store) ->
    receive
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store);

        {notify, New} ->
            {Pred, NewStore} = notify(New, MyKey, Predecessor, Store),
            node(MyKey, Pred, Successor, NewStore);

        {request, Peer} ->
            request(Peer, Predecessor),
            node(MyKey, Predecessor, Successor, Store);

        {status, Pred} ->
            Succ = stabilize(Pred, MyKey, Successor),
            node(MyKey, Predecessor, Succ, Store);

        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store);

        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);

        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store);

        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey | Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);

       {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, Merged)
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.


notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Store, MyKey, Nkey, Npid),
            { {Nkey, Npid}, Keep };                                 %% I'm my self predecessor

        {Pkey, _} ->
            io:format("Nkey ~w, Pkey ~w, MyKey ~w ~n",[Nkey, Pkey, MyKey]),
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    Keep = handover(Store, MyKey, Nkey, Npid),      %% Forward the rest of list
                    { {Nkey, Npid}, Keep };                         %% I'm new predecessor
                false ->
                    {Predecessor, Store}
        end
    end.

create_probe(MyKey, {_, Spid}, Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w!~n Store: ~w~n", [MyKey, Store]).

remove_probe(MyKey, Nodes, T) ->
    Time = timer:now_diff(erlang:now(), T),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(RefKey, Nodes, T, {_, Spid}, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n Store: ~w~n", [RefKey, Store]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of               %% Take care of predecessor keys and my own key.
        true ->
            Added = storage:add(Key, Value, Store),     %% Adding word to store
            Client ! {Qref, ok},
            Added;

        false ->
            Spid ! {add, Key, Value, Qref, Client},     %% Sending data to my succesor
            Store
    end.

lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of               %% Check if I takes care of the key
        true ->
            Result = storage:lookup(Key, Store),        %% Finding key in Store
            Client ! {Qref, Result};

        false ->
            Spid ! {lookup, Key, Qref, Client}          %% Forward lookup
    end.

%% for list [1,2,3,4,5,6] when MyKey eq 6 and Nkey eq 4
%%      handover(...): Keep = [5, 6], Leave = [1, 2, 3, 4]
handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.
