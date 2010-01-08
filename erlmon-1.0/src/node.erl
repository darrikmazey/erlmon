
-module(node).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

start() ->
	case whereis(node) of
		undefined ->
			register(node, Pid = spawn_link(?MODULE, init, []));
		Pid ->
			false
	end,
	{ok, Pid}.

init() ->
	debug:log("node: initializing"),
	loop([]).

loop(State) ->
	receive
		_ -> loop(State)
	end.
