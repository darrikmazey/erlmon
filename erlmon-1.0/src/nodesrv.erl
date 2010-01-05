
-module(nodesrv).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-export([add_node/1]).

-include("include/nodesrv.hrl").

start() ->
	case global:whereis_name(nodesrv) of
		undefined ->
			global:register_name(nodesrv, Pid = spawn_link(nodesrv, init, []));
		Pid ->
			false
	end,
	case whereis(nodesrv) of
		undefined ->
			register(nodesrv, Pid);
		LPid ->
			false
	end,
	{ok, Pid}.

init() ->
	debug:log("nodesrv: initializing"),
	loop([]).

loop(State) ->
	receive
		#msg_nodesrv_add_node{sender=Sender, node=Node} ->
			debug:log("received add ~p from ~p", [Node, Sender]),
			loop(State);
		_ -> loop(State)
	end.

add_node(Node) when is_atom(Node) ->
	global:whereis_name(nodesrv) ! #msg_nodesrv_add_node{sender=self(), node=Node},
	ok.

