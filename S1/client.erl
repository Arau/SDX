-module(client).

%% Exported Functions
-export([start/2, init_client/2]).

%% API Functions
start(ServerPid, MyName) ->
    ClientPid = spawn(client, init_client, [ServerPid, MyName]),
    process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
    ServerPid ! {client_join_req, MyName, self() },  %% Join client to server
    process_requests().

%% Local Functions
%% This is the background task logic
process_requests() ->
    receive
        {join, Name} ->
            io:format("[JOIN] ~s joined the chat~n", [Name]),
            process_requests(); %% Listen remaining

        {leave, Name} ->
            io:format("[LEAVE] ~s leaved the chat~n", [Name]),  %% Print leave message
            process_requests();

        {message, Name, Text} ->
            io:format("[~s] ~s~n", [Name, Text]),
            process_requests();  %% Listen remaining

        exit ->
            ok
    end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
    %% Read from standard input and send to server
    Text = io:get_line("-> "),
    if
        Text  == "exit\n" ->
            ServerPid ! {client_leave_req, MyName, ClientPid},  %% Notify my logoff
            ok;

        true ->
            ServerPid ! {send, MyName, Text},
            process_commands(ServerPid, MyName, ClientPid)
end.
