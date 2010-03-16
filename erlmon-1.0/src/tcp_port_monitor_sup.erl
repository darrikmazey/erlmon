
-module(tcp_port_monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([monitor/2]).
-export([unmonitor/2]).

monitor(Host, Port) ->
	debug:log("tcp_port_monitor_sup:monitor(~p, ~p)", [Host, Port]),
	supervisor:start_child(tcp_port_monitor_sup, [Host, Port]).

unmonitor(Host, Port) ->
	supervisor:terminate_child(tcp_port_monitor_sup, [Host, Port]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	erlmon:finished(?MODULE),
	R.

init(_) ->
	debug:log("tcp_port_monitor_sup: starting"),
	TPMChild = {tcp_port_monitor, {tcp_port_monitor, start, []}, temporary, brutal_kill, worker, [tcp_port_monitor]},
	{ok, {{simple_one_for_one, 0, 1}, [TPMChild]}}.
