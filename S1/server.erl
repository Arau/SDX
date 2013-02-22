-module(server).

%% Exported Functions
-export([start/0, process_requests/1]).

%% API Functions
start() ->
    ServerPid = spawn(server, process_requests, [ [] ]),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [ {Name, From} | Clients],  %% Add new client to the list
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  %% Listen remaining

        {client_leave_req, Name, From} ->
            NewClients = lists:delete({Name, From}, Clients),  %% Delete client from list
            broadcast(Clients, {leave, Name}),  %% Notify user logoff to rest of clients
            From ! exit,
            process_requests(NewClients);  %% Listen remaining

        {send, Name, Text} ->
            broadcast(Clients, {message, Name, Text}),  %% Message propagate
            process_requests(Clients);

        disconnect ->
            unregister(myserver)
end.

%% Local Functions
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
