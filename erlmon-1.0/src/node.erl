
-module(node).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-include("include/node.hrl").
-include("include/state_mon.hrl").

start() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("node: initializing"),
	State = net_adm:world(),
	debug:log("initial state: ~p", [State]),
	NewState = start_monitoring_nodes(State),
	announce(NewState, NewState),
	debug:log("final state: ~p", [NewState]),
	loop(NewState).

loop(State) ->
	debug:log("LOOP~n~p", [State]),
	receive
		#node_announce{sender=_Sender, pid=Pid, node=Node, state=TheirState} ->
			debug:log("received announce for ~p on ~p~n~p", [Pid, Node, TheirState]),
			NewState = set_node_up(Node, State),
			case TheirState == NewState of
				false ->
					debug:log("different states!");
				true ->
					debug:log("same states!")
			end,
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
			{Node, up}
	end.

set_node_down(Node, [{Node, up}|T]) ->
	debug:log("setting node down for ~p", [Node]),
	state_mon ! #state_change{sender=self(), objtype=node, obj=Node, prev_state=up, new_state=down},
	[{Node, down}|T];
set_node_down(Node, [{Node, down}|T]) ->
	debug:log("setting node down for ~p (already down)", [Node]),
	[{Node, down}|T];
set_node_down(Node, [H|T]) ->
	[H|set_node_down(Node, T)];
set_node_down(Node, []) ->
	state_mon ! #state_change{sender=self(), objtype=node, obj=Node, prev_state=none, new_state=down},
	[{Node, down}].

set_node_up(Node, [{Node, down}|T]) ->
	debug:log("setting node up for ~p", [Node]),
	state_mon ! #state_change{sender=self(), objtype=node, obj=Node, prev_state=down, new_state=up},
	start_monitoring_node(Node),
	[{Node, up}|T];
set_node_up(Node, [{Node, up}|T]) ->
	debug:log("setting node up for ~p (already up)", [Node]),
	[{Node, up}|T];
set_node_up(Node, [H|T]) ->
	[H|set_node_up(Node, T)];
set_node_up(Node, []) ->
	state_mon ! #state_change{sender=self(), objtype=node, obj=Node, prev_state=none, new_state=up},
	[start_monitoring_node(Node)].

announce([{Node, up}|T], State) ->
	case Node == node() of
		false ->
			Pid = find_node_pid(Node),
			debug:log("announcing to ~p on ~p", [Pid, Node]),
			Pid ! #node_announce{sender=self(), pid=self(), node=node(), state=State},
			announce(T, State);
		true ->
			debug:log("not announcing to self")
	end;
announce([], _State) ->
	[].

find_node_pid(Node) when is_atom(Node) ->
	rpc:call(Node, erlang, whereis, [node]).
