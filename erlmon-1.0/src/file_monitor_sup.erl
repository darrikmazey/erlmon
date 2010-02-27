
-module(file_monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([monitor/1]).

monitor(Path) ->
	supervisor:start_child(file_monitor_sup, [Path]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("file_monitor_sup: starting up"),
	FMSChild = {file_monitor, {file_monitor, start, []}, temporary, brutal_kill, worker, [file_monitor]},
	{ok, {{simple_one_for_one, 0, 1}, [FMSChild]}}.
