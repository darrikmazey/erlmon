
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
	erlmon_sup:start_link().

init(_) ->
	application:set_env(erlwww, port, 8000),
	application:start(erlwww),
	state_change_sup:start_link(),
	state_change_em:add_handler(state_change_handler),
	state_change_em:add_handler(node_down_handler),
  load().

load() -> 
  file_monitor:monitor("config.lua"),
  ok.


stop(_State) ->
	debug:log("erlmon: stopping"),
	ok.
