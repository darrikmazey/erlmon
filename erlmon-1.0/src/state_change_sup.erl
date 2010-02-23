
-module(state_change_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("state_change_sup: starting state_change_em"),
	Child = {state_change_em, {state_change_em, start_link, []}, permanent, 2000, worker, [state_change_em]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.
