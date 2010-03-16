
-module(node).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).
-export([monitor_node/1]).
-export([status/0]).

-include("include/erlmon.hrl").

start() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("node: initializing"),
	State = net_adm:world(),
	NewState = start_monitoring_nodes(State),
	announce(NewState, NewState),
	erlmon:finished(?MODULE),
	loop(NewState).

loop(State) ->
	receive
		{Sender, _Status} ->
			debug:log("node: received status request"),
			Sender ! {ok, ok},
			loop(State);
		_Msg = #node_announce{sender=_Sender, pid=Pid, node=Node, state=TheirState} ->
			debug:log("received announce for ~p on ~p~n~p", [Pid, Node, TheirState]),
			start_monitoring_node(Node),
			loop(State);
		M ->
			debug:log("node:UNKNOWN: ~p", [M]),
			loop(State)
	end.

start_monitoring_nodes([Node|T]) ->
	[start_monitoring_node(Node)|start_monitoring_nodes(T)];
start_monitoring_nodes([]) -> [].

start_monitoring_node(Node) ->
	case Node == node() of
		false ->
			%erlang:monitor_node(Node, true),
			%debug:log("monitoring node: ~p", [Node]),
			spawn(node, monitor_node, [Node]),
			{Node, up};
		true ->
			{Node, up}
	end.

announce([{Node, up}|T], State) ->
	case Node == node() of
		false ->
			Pid = find_node_pid(Node),
			case Pid of
				undefined ->
					debug:log("can not announce to node ~p: undefined Pid", [Node]);
				_ ->
				debug:log("announcing to ~p on ~p", [Pid, Node]),
				Pid ! #node_announce{sender=self(), pid=self(), node=node(), state=State}
			end,
			announce(T, State);
		true ->
			debug:log("not announcing to self"),
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=?MODULE, obj=node(), prev_state=down, new_state=up, ts=timestamp:now_i()})
	end;
announce([], _State) ->
	[].

find_node_pid(Node) when is_atom(Node) ->
	rpc:call(Node, erlang, whereis, [node]).

monitor_node(Node) ->
	debug:log("node: monitoring ~p (~p)", [Node, self()]),
	state_change_em:notify(#state_change{sender=self(), node=node(), objtype=?MODULE, obj=Node, prev_state=down, new_state=up, ts=timestamp:now_i()}),
	erlang:monitor_node(Node, true),
	receive
		{nodedown, Node} ->
			debug:log("node: received nodedown for ~p (~p)", [Node, self()]),
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=?MODULE, obj=Node, prev_state=up, new_state=down, ts=timestamp:now_i()});
		M ->
			debug:log("node:UNKNOWN: ~p (~p)", [M, self()])
	end.

status() ->
	node ! {self(), status},
	receive
		{ok, Status} -> ok
	end,
	Status.
