-module(ping_pong_distributed).
-export([start_ping/1, start_pong/0, ping/2, pong/0]).

ping(0, Pong_Node) ->
	{pong, Pong_Node} ! finished,
	io:format("ping finished~n",[]);

ping(N, Pong_Node) -> 
	{pong, Pong_Node} ! {ping, self()},
	receive 
		pong -> 
			io:format("ping received pong~n", [])
	end,
	ping(N - 1, Pong_Node).

pong() ->
	receive 
		finished -> 
			io:format("Pong finished~n" , []);

		{ping, Ping_PID} ->
			io:format("Pong received ping~n", []),
			Ping_PID ! pong,
			pong()
	end.

start_pong() ->
	Proc_Pong =  spawn(ping_pong_distributed, pong, []),
	register(pong, Proc_Pong).

start_ping(Pong_Node) ->
	spawn(ping_pong_distributed, ping, [3, Pong_Node]).


