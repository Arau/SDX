-module(conversation).
-export([hello/0]).

hello() ->
	receive 
		X -> io:format("Receive message from: ~s~n", [X])
	end.
