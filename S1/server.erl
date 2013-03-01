-module(server).

%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1]).

%% API Functions
start() ->
    ServerPid = spawn(server, init_server, []),
    register(myserver, ServerPid).

start(BootServer) ->
    ServerPid = spawn(server, init_server, [BootServer]),
    register(myserver, ServerPid).

init_server() ->
    process_requests([], [ self() ]).

init_server(BootServer) ->
    BootServer ! {server_join_req, self()},
    process_requests([], []).

process_requests(Clients, Servers) ->
    receive
        %% Client-Server communication

        {client_join_req, Name, From} ->
            NewClients = [From | Clients],                      %% Add new client to the list
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients, Servers);              %% Listen remaining

        {client_leave_req, Name, From} ->
            NewClients = lists:delete({Name, From}, Clients),   %% Delete client from list
            broadcast(Clients, {leave, Name}),                  %% Notify user logoff to rest of clients
            From ! exit,
            process_requests(NewClients, Servers);              %% Listen remaining

        {send, Name, Text} ->
            broadcast(Servers, {message, Name, Text}),          %% Message propagate
            process_requests(Clients, Servers);


        %% Server-Server communication

        disconnect ->
            NewServers = lists:delete(self(), Servers),
            broadcast(NewServers, {update_servers, NewServers}), %% Update list of servers
            unregister(myserver);

        {server_join_req, From} ->
            NewServers = [From | Servers],                 %% Add new server to list
            broadcast(NewServers, {update_servers, From}), %% Notify to servers new connected server due to propague msg's
            broadcast(Clients,    {update_servers, From}), %% Notify to clients new connected server due to failover
            process_requests(Clients, NewServers);

        {update_servers, NewServers} ->
            io:format("[SERVER UPDATE] ~w~n", [NewServers]),
            process_requests(Clients, NewServers);         %% Listen remaining

        RelayMessage ->                                    %% Whatever other message is relayed to its clients
            broadcast(Clients, RelayMessage),
            process_requests(Clients, Servers)
    end.

%% Local Functions
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
