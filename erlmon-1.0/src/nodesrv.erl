
-module(nodesrv).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0, init/1]).

-export([add_node/1]).
-export([remove_node/1]).
-export([list_nodes/0]).
-export([declare_node/1]).

-include("include/nodesrv.hrl").

start() ->
	Nodes = net_adm:world(),
	register(nodesrv, Pid = spawn_link(nodesrv, init, [Nodes])),
	{ok, Pid}.

init() ->
	debug:log("nodesrv: initializing"),
	loop([]).

init(Nodes) ->
	debug:log("nodesrv: initializing with nodes (~p)", [Nodes]),
	State = find_nodesrv_on_nodes(Nodes),
	announce(State),
	debug:log("initial state: ~p", [State]),
	loop(State).

loop(State) ->
	receive
		#msg_nodesrv_list_nodes{sender=Sender} ->
			Sender ! #msg_nodesrv_node_list{sender=self(), nodes=State},
			loop(State);
		#msg_nodesrv_add_node{sender=Sender, node=Node} ->
			debug:log("received add ~p from ~p", [Node, Sender]),
			NewState = add_node_to_state(Node, State),
			loop(NewState);
		#msg_nodesrv_rem_node{sender=Sender, node=Node} ->
			debug:log("received remove ~p from ~p", [Node, Sender]),
			NewState = remove_node_from_state(Node, State),
			loop(NewState);
		#msg_nodesrv_announce{sender=Sender, node=Node, nodesrv=Nodesrv} ->
			debug:log("received announce from ~p for ~p on ~p", [Sender, Nodesrv, Node]),
			NewState = add_node_to_state(Node, State),
			loop(NewState);
		_ -> loop(State)
	end.

list_nodes() ->
	nodesrv ! #msg_nodesrv_list_nodes{sender=self()},
	receive
		#msg_nodesrv_node_list{sender=_Sender, nodes=NodeList} ->
			NodeList
	end.

add_node(Node) when is_atom(Node) ->
	nodesrv ! #msg_nodesrv_add_node{sender=self(), node=Node},
	ok.

remove_node(Node) when is_atom(Node) ->
	nodesrv ! #msg_nodesrv_rem_node{sender=self(), node=Node},
	ok.

declare_node(Sender) when is_pid(Sender) ->
	Sender ! {self(), whereis(nodesrv)}.

find_nodesrv_on_nodes([Node|T]) ->
	[find_nodesrv_on_node(Node)|find_nodesrv_on_nodes(T)];
find_nodesrv_on_nodes([]) -> [].

find_nodesrv_on_node(Node) ->
	{Node, rpc:call(Node, erlang, whereis, [nodesrv])}.

add_node_to_state(Node, [{Node, Pid}|T]) when is_pid(Pid) ->
	[{Node, Pid}|T];
add_node_to_state(Node, [H|T]) ->
	[H|add_node_to_state(Node, T)];
add_node_to_state(Node, []) ->
	case net_adm:ping(Node) of
		pang -> [];
		pong -> [find_nodesrv_on_node(Node)|[]]
	end.

remove_node_from_state(Node, [{Node,_Pid}|T]) ->
	[T];
remove_node_from_state(Node, [H|T]) ->
	[H|remove_node_from_state(Node, T)];
remove_node_from_state(_Node, []) -> [].

announce([{_Node, Pid}|T]) ->
	Pid ! #msg_nodesrv_announce{sender=self(), node=node(), nodesrv=whereis(nodesrv)},
	announce(T);
announce([]) -> ok.
