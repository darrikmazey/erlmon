
-module(state_change_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	wait_for_state_change_em(),
	R.


init(_) ->
	debug:log("state_change_sup: starting state_change_em"),
	Child = {state_change_em, {state_change_em, start_link, []}, permanent, 2000, worker, [state_change_em]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.

wait_for_state_change_em() ->
	case whereis(state_change_em) of
		undefined ->
			wait_for_state_change_em();
		_ -> add_default_handlers()
	end.

add_default_handlers() ->
	state_change_em:add_handler(state_change_handler),
	state_change_em:add_handler(config_file_change_handler),
	state_change_em:add_handler(node_down_handler).
