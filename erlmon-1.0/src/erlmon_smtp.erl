
-module(erlmon_smtp).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).

-include("include/erlmon.hrl").

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("erlmon_smtp: initializing"),
	state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp, obj=self(), prev_state=down, new_state=disabled, ts=timestamp:now_i()}),
	loop([]).

loop(State) ->
	receive
		M ->
			debug:log("erlmon_smtp:UNKNOWN: ~p", [M]),
			loop(State)
	end.

