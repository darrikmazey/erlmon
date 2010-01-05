
-module(erlmon_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("erlmon_sup: starting"),
	NSChild = {nodesrv_sup, {nodesrv_sup, start_link, []}, permanent, 2000, supervisor, [nodesrv_sup]},
	NChild = {node_sup, {node_sup, start_link, []}, permanent, 2000, supervisor, [node_sup]},
	{ok, {{one_for_one, 1, 1}, [NSChild,NChild]}}.
