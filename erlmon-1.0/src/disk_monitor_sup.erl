
-module(disk_monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("disk_monitor_sup: starting up"),
	DMSChild = {disk_monitor, {disk_monitor, start, []}, temporary, brutal_kill, worker, [disk_monitor]},
	{ok, {{simple_one_for_one, 0, 1}, [DMSChild]}}.
