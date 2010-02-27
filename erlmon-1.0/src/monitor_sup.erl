
-module(monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("monitor_sup: starting disk_monitor_sup"),
	DMSChild = {disk_monitor_sup, {disk_monitor_sup, start_link, []}, permanent, 2000, supervisor, [disk_monitor_sup]},
	FMSChild = {file_monitor_sup, {file_monitor_sup, start_link, []}, permanent, 2000, supervisor, [file_monitor_sup]},
	FMMChild = {file_monitor_man, {file_monitor_man, start_link, []}, permanent, 2000, worker, [file_monitor_man]},
	{ok, {{one_for_one, 1, 1}, [DMSChild, FMSChild, FMMChild]}}.
