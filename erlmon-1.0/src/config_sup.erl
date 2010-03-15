
-module(config_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("config_sup: starting config"),
	Child = {config, {config, start_link, []}, permanent, 2000, worker, [config]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.
