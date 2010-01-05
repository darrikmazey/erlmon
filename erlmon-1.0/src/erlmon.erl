
-module(erlmon).
-author(darrik@darmasoft.com).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	debug:log_to(file, {filename, "erlmon.log"}),
	debug:log("erlmon: initializing"),
	erlmon_sup:start_link().

stop(_State) ->
	debug:log("erlmon: stopping"),
	ok.
