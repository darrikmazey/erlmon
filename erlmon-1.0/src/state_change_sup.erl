
-module(state_change_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-include("include/erlmon.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	wait_for_state_change_em(),
	erlmon:finished(?MODULE),
	R.


init(_) ->
	debug:log("state_change_sup: starting state_change_em"),
	Child = {state_change_em, {state_change_em, start_link, []}, permanent, 2000, worker, [state_change_em]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.

wait_for_state_change_em() ->
	case whereis(state_change_em) of
		undefined ->
			wait_for_state_change_em();
		_ ->
			add_default_handlers(),
			add_default_filters()
	end.

add_default_handlers() ->
	state_change_em:add_handler(state_change_handler),
	state_change_em:add_handler(config_file_change_handler),
	state_change_em:add_handler(node_down_handler),
	state_change_em:add_handler(alert_handler).

add_default_filters() ->
	SC = #state_change{sender=nil, node=nil, objtype=file, obj="config.lua", prev_state=unchanged, new_state=file_ctime_changed, ts=nil},
	SCF = #state_change_filter{state_change=SC, fields=[objtype, obj, prev_state, new_state]},
	AF = #alert_filter{scf=SCF, to="darrik@darmasoft.com"},
	alert_handler:register_alert_filter(AF).
