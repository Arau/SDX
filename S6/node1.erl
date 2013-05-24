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


-define(Stabilize, 1000).

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize(Pred, MyKey, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            %% TODO: ADD SOME CODE
            Successor;
        {MyKey, _} ->
            Successor;
        {Skey, _} ->
            %% TODO: ADD SOME CODE
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    %% TODO: ADD SOME CODE
                    %% TODO: ADD SOME CODE
                false ->
                    %% TODO: ADD SOME CODE
                    Successor
            end
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.


-define(Timeout, 5000).

connect(MyKey, nil) ->
    {ok, {... , ...}}; %% TODO: ADD SOME CODE

connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},

    receive
        {Qref, Skey} ->
            {ok, {... , ...}} %% TODO: ADD SOME CODE

    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])

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
            %% TODO: ADD SOME CODE

        {Pkey, _} ->
            case key:between(Nkey, Pkey, MyKey) of
            true ->
                %% TODO: ADD SOME CODE
            false ->
                Predecessor
        end
    end.
