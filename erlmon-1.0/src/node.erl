
-module(node).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-include("include/node.hrl").

start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("node: initializing"),
	State = net_adm:world(),
	debug:log("initial state: ~p", [State]),
	NewState = start_monitoring_nodes(State),
	announce(NewState),
	debug:log("final state: ~p", [NewState]),
	loop(NewState).

loop(State) ->
	debug:log("LOOP~n~p", [State]),
	receive
		#node_announce{sender=_Sender, pid=Pid, node=Node} ->
			debug:log("received announce for ~p on ~p", [Pid, Node]),
			NewState = set_node_up(Node, State),
			loop(NewState);
		{nodedown, Node} ->
			debug:log("received nodedown for ~p", [Node]),
			NewState = set_node_down(Node, State),
			loop(NewState);
		M ->
			debug:log("UNKNOWN: ~p", [M]),
			loop(State)
	end.

start_monitoring_nodes([Node|T]) ->
	[start_monitoring_node(Node)|start_monitoring_nodes(T)];
start_monitoring_nodes([]) -> [].

start_monitoring_node(Node) ->
	case Node == node() of
		false ->
			erlang:monitor_node(Node, true),
			debug:log("monitoring node: ~p", [Node]),
			{Node, up};
		true ->
			{Node, self}
	end.

set_node_down(Node, [{Node, up}|T]) ->
	debug:log("setting node down for ~p", [Node]),
	[{Node, down}|T];
set_node_down(Node, [{Node, down}|T]) ->
	debug:log("setting node down for ~p (already down)", [Node]),
	[{Node, down}|T];
set_node_down(Node, [H|T]) ->
	[H|set_node_down(Node, T)];
set_node_down(Node, []) ->
	[{Node, down}].

set_node_up(Node, [{Node, down}|T]) ->
	debug:log("setting node up for ~p", [Node]),
	[{Node, up}|T];
set_node_up(Node, [{Node, up}|T]) ->
	debug:log("setting node up for ~p (already up)", [Node]),
	[{Node, up}|T];
set_node_up(Node, [H|T]) ->
	[H|set_node_up(Node, T)];
set_node_up(Node, []) ->
	[start_monitoring_node(Node)].

announce([{_Node, self}|T]) ->
	debug:log("not announcing to self"),
	announce(T);
announce([{Node, up}|T]) ->
	Pid = find_node_pid(Node),
	debug:log("announcing to ~p on ~p", [Pid, Node]),
	Pid ! #node_announce{sender=self(), pid=self(), node=node()},
	announce(T);
announce([]) ->
	[].

find_node_pid(Node) when is_atom(Node) ->
	rpc:call(Node, erlang, whereis, [node]).
