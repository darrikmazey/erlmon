
-module(storage).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-export([announce/2]).

-export([state_change/1]).

-export([test/2]).
-export([status/0]).

-include("include/erlmon.hrl").

start() ->
	debug:log_to(console, {}),
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("storage: initializing"),
	debug:log("storage: touching world"),
	Nodes = net_adm:world(),
	debug:log("storage: found ~p", [Nodes]),
	NodeCount = length(Nodes),
	case NodeCount > 1 of
		true ->
			debug:log("storage: we are not the first; announcing"),
			init_storage(false),
			announce_storage_to_nodes(Nodes);
		false ->
			debug:log("storage: we are first; initializing"),
			init_storage(true)
	end,
	loop([]).

loop(State) ->
	receive
		{Sender, status} ->
			debug:log("storage: received status request"),
			Sender ! {ok, ok},
			loop(State);
		Msg = #state_change{} ->
			debug:log("storage: received state_change"),
			store(Msg),
			loop(State);
		#storage_ack_announce{sender=Sender, node=Node} ->
			debug:log("storage: received ACK announce from ~p on ~p", [Sender, Node]),
			loop(State);
		#storage_announce{sender=Sender, node=Node} ->
			debug:log("storage: received announce from ~p on ~p", [Sender, Node]),
			copy_storage_to_node(Node),
			Sender ! #storage_ack_announce{sender=self(), node=node()},
			loop(State);
		M ->
			debug:log("storage: UNKNOWN: ~p", [M]),
			loop(State)
	end.

announce(Sender, Node) ->
	storage ! #storage_announce{sender=Sender, node=Node},
	ok.

announce_storage_to_nodes([Node|T]) when Node =:= node() ->
	debug:log("storage: not announcing to self"),
	announce_storage_to_nodes(T);
announce_storage_to_nodes([Node|T]) ->
	debug:log("storage: announcing to ~p", [Node]),
	Pid = rpc:call(Node, erlang, whereis, [storage]),
	debug:log("storage: found storage at ~p on ~p", [Pid, Node]),
	Pid ! #storage_announce{sender=self(), node=node()},
	announce_storage_to_nodes(T);
announce_storage_to_nodes([]) -> ok.

init_storage(false) ->
	debug:log("storage: initializing storage"),
	mnesia:start();
init_storage(true) ->
	init_storage(false),
	debug:log("storage: initializing schema"),
	mnesia:change_table_copy_type(schema, node(), ram_copies),
	create_tables(),
	wait_for_tables().

create_tables() ->
	mnesia:create_table(test, [{attributes, record_info(fields, test)}, {type, bag}, {ram_copies, [node()]}]),
	mnesia:create_table(state_change, [{attributes, record_info(fields, state_change)}, {type, bag}, {ram_copies, [node()]}]).

copy_tables(Node) ->
	_R1 = mnesia:add_table_copy(test, Node, ram_copies),
	_R2 = mnesia:add_table_copy(state_change, Node, ram_copies).

wait_for_tables() ->
	mnesia:wait_for_tables([test, state_change], 10000).
	
copy_storage_to_node(Node) ->
	debug:log("storage: copying storage to ~p", [Node]),
	_R1 = mnesia:change_config(extra_db_nodes, [Node]),
	_R2 = mnesia:change_table_copy_type(schema, Node, ram_copies),
	copy_tables(Node).

test(K, V) ->
	Row = #test{key=K, value=V},
	F = fun() ->
		mnesia:write(Row)
	end,
	mnesia:transaction(F).

state_change(Msg) ->
	storage ! Msg.

store(Msg) ->
	F = fun() -> mnesia:write(Msg) end,
	mnesia:transaction(F).

status() ->
	storage ! {self(), status},
	receive
		{ok, Status} -> ok
	end,
	Status.
