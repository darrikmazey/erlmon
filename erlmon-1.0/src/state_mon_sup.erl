
-module(state_mon_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("state_mon_sup: starting node"),
	Child = {state_mon, {state_mon, start, []}, permanent, 2000, worker, [state_mon]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.
