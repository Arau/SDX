-module(ping_pong_named).
-export([start/0, ping/1, pong/0]).

ping(0) ->
	pong ! finished,
	io:format("ping finished~n",[]);

ping(N) -> 
	pong ! {ping, self()},
	receive 
		pong -> 
			io:format("ping received pong~n", [])
	end,
	ping(N - 1).

pong() ->
	receive 
		finished -> 
			io:format("Pong finished~n" , []);

		{ping, Ping_PID} ->
			io:format("Pong received ping~n", []),
			Ping_PID ! pong,
			pong()
	end.

start() ->
	register(pong, spawn(ping_pong_named, pong, [])),
	spawn(ping_pong_named, ping, [3]).


