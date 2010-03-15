
-module(erlmon_smtp_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	debug:log("erlmon_smtp_sup: starting"),
	GChild = {erlmon_smtp, {erlmon_smtp, start_link, []}, permanent, 2000, worker, [erlmon_smtp]},
	{ok, {{one_for_one, 1, 1}, [GChild]}}.
