
-module(erlmon).
-author(darrik@darmasoft.com).

-behaviour(application).

-include("include/erlmon.hrl").

-export([start/0, start/2, stop/1, finished/1]).

-define(SUPERVISORS, [
	erlmon_sup,
	state_change_sup,
	state_change_em,
	storage_sup,
	storage,
	node_sup,
	node,
	monitor_sup,
	disk_monitor_sup,
	tcp_port_monitor_sup,
	tcp_port_monitor_man,
	file_monitor_sup,
	file_monitor_man,
	process_list_sup,
	process_monitor_sup,
	process_monitor_man,
	config_sup,
	config,
	erlmon_smtp_sup
]).

start() ->
	application:start(erlmon).

start(_Type, _StartArgs) ->
	debug:log_to(file, {filename, "erlmon.log"}),
	debug:log_to(console, {}),
	debug:log("erlmon: initializing"),
	register(erlmon, self()),
	R = erlmon_sup:start_link(),
	wait_for_finished(?SUPERVISORS),
  load(),
	R.

load() -> 
  file_monitor:monitor("config.lua"),
  config:reload(),
	erlmon_smtp:config(),
	application:set_env(erlwww, port, 8000),
	application:start(erlwww),
  ok.


stop(_State) ->
	debug:log("erlmon: stopping"),
	ok.

wait_for_finished([]) ->
	debug:log("erlmon: startup complete"),
	ok;
wait_for_finished([S|T]) ->
	debug:log("erlmon: waiting for ~p", [S]),
	receive
		{S, started} -> 
			debug:log("erlmon: ~p finished", [S]),
			ok
	end,
	wait_for_finished(T).

finished(S) ->
	erlmon ! {S, started}.
