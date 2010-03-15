
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
	application:start(erlwww).

load() -> 
  file_monitor:monitor("config.lua"),
  config:reload(),
  ok.


stop(_State) ->
	debug:log("erlmon: stopping"),
	ok.

