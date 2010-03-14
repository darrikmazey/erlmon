
-module(process_monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([monitor/1]).
-export([unmonitor/1]).

monitor(ProcessName) ->
	debug:log("process_monitor_sup:monitor(~p)", [ProcessName]),
	supervisor:start_child(process_monitor_sup, [ProcessName]).

unmonitor(ProcessName) ->
	supervisor:terminate_child(tcp_port_monitor_sup, [ProcessName]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("process_monitor_sup: starting"),
	TPMChild = {process_monitor, {process_monitor, start, []}, temporary, brutal_kill, worker, [process_monitor]},
	{ok, {{simple_one_for_one, 0, 1}, [TPMChild]}}.
