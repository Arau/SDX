-module(wait).
-export([hello/0]).

hello() ->
	receive 
		X -> io:format("aaa! surpriese, a msg : ~s~n", [X])
	end.
