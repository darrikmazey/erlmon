
-module(node_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	wait_for_node(),
	erlmon:finished(?MODULE),
	R.

init(_) ->
	debug:log("node_sup: starting node"),
	Child = {node, {node, start, []}, permanent, 2000, worker, [node]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.

wait_for_node() ->
	node:status().
