
-module(erlmon_smtp_gateway).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).

-include("include/erlmon.hrl").

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("erlmon_smtp_gateway: initializing"),
	net_adm:world(),
	case global:whereis_name(erlmon_smtp_gateway) of
		undefined ->
			%% first one up, become primary
			debug:log("erlmon_smtp_gateway: first one up, becoming primary"),
			case global:register_name(erlmon_smtp_gateway, self()) of
				yes ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp_gateway, obj=self(), prev_state=down, new_state=primary, ts=timestamp:now_i()});
				no ->
					erlang:monitor(process, global:whereis_name(erlmon_smtp_gateway))
			end;
		M ->
			erlang:monitor(process, M),
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp_gateway, obj=self(), prev_state=down, new_state=secondary, ts=timestamp:now_i()})
	end,
	loop([]).

loop(State) ->
	receive
		{'DOWN', _Ref, process, Pid, _Reason} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp_gateway, obj=Pid, prev_state=primary, new_state=down, ts=timestamp:now_i()}),
			case global:register_name(erlmon_smtp_gateway, self()) of
				yes ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp_gateway, obj=self(), prev_state=secondary, new_state=primary, ts=timestamp:now_i()});
				no ->
				  erlang:monitor(process, global:whereis_name(erlmon_smtp_gateway))
			end;
		M ->
			debug:log("erlmon_smtp_gateway:UNKNOWN: ~p", [M]),
			loop(State)
	end.

