
-module(erlmon).
-author(darrik@darmasoft.com).

-behaviour(application).

-export([start/0, start/2, stop/1, init/1]).

start() ->
	application:start(erlmon).

start(_Type, _StartArgs) ->
	debug:log_to(file, {filename, "erlmon.log"}),
	debug:log_to(console, {}),
	debug:log("erlmon: initializing"),
	init([]),
	R = erlmon_sup:start_link(),
  load(),
	R.

init(_) ->
	application:set_env(erlwww, port, 8000),
	application:start(erlwww),
	state_change_sup:start_link(),
	wait_for_state_change_em(),
	state_change_em:add_handler(state_change_handler),
	state_change_em:add_handler(config_file_change_handler),
	state_change_em:add_handler(node_down_handler).

load() -> 
  file_monitor:monitor("config.lua"),
  config:reload(),
  ok.


stop(_State) ->
	debug:log("erlmon: stopping"),
	ok.

wait_for_state_change_em() ->
	case whereis(state_change_em) of
		undefined ->
			wait_for_state_change_em();
		_ -> ok
	end.
