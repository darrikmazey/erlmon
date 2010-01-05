
-module(nodesrv_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("nodesrv_sup: starting nodesrv"),
	Child = {nodesrv, {nodesrv, start, []}, permanent, 2000, worker, [nodesrv]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.
