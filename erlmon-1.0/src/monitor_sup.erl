
-module(monitor_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	erlmon:finished(?MODULE),
	R.

init(_) ->
	debug:log("monitor_sup: starting disk_monitor_sup"),
	DMSChild = {disk_monitor_sup, {disk_monitor_sup, start_link, []}, permanent, 2000, supervisor, [disk_monitor_sup]},
	FMSChild = {file_monitor_sup, {file_monitor_sup, start_link, []}, permanent, 2000, supervisor, [file_monitor_sup]},
	FMMChild = {file_monitor_man, {file_monitor_man, start_link, []}, permanent, 2000, worker, [file_monitor_man]},
	TPMSChild = {tcp_port_monitor_sup, {tcp_port_monitor_sup, start_link, []}, permanent, 2000, supervisor, [tcp_port_monitor_sup]},
	TPMMChild = {tcp_port_monitor_man, {tcp_port_monitor_man, start_link, []}, permanent, 2000, worker, [tcp_port_monitor_man]},
	PMSChild = {process_monitor_sup, {process_monitor_sup, start_link, []}, permanent, 2000, supervisor, [process_monitor_sup]},
	PMMChild = {process_monitor_man, {process_monitor_man, start_link, []}, permanent, 2000, worker, [process_monitor_man]},
	{ok, {{one_for_one, 1, 1}, [DMSChild, FMSChild, FMMChild, TPMSChild, TPMMChild, PMSChild, PMMChild]}}.
