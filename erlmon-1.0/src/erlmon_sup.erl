
-module(erlmon_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("erlmon_sup: starting"),
	NChild = {node_sup, {node_sup, start_link, []}, permanent, 2000, supervisor, [node_sup]},
	%SMChild = { state_change_sup, {state_change_sup, start_link, []}, permanent, 2000, supervisor, [state_change_sup]},
	SChild = { storage_sup, {storage_sup, start_link, []}, permanent, 2000, supervisor, [storage_sup]},
	MSChild = { monitor_sup, {monitor_sup, start_link, []}, permanent, 2000, supervisor, [monitor_sup]},
	SGChild = { erlmon_smtp_sup, {erlmon_smtp_sup, start_link, []}, permanent, 2000, supervisor, [erlmon_smtp_sup]},
	{ok, {{one_for_one, 1, 1}, [SChild, NChild, MSChild, SGChild]}}.
