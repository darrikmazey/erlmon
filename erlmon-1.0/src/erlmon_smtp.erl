
-module(erlmon_smtp).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).
-export([status/0]).
-export([config/1]).

-include("include/erlmon.hrl").

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("erlmon_smtp: initializing"),
	state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp, obj=self(), prev_state=down, new_state=disabled, ts=timestamp:now_i()}),
	loop({disabled, nil}).

loop({OldStatus, _OldConfig}=State) ->
	receive
		{Sender, status} ->
			{Status, _Config} = State,
			Sender ! {status, Status},
			loop(State);
		{Sender, config, #smtp_config{}=C} ->
			debug:log("erlmon_smtp:config: ~p", [C]),
			{NewStatus, NewConfig} = set_config(State, C),
			Sender ! {ok, NewStatus},
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=smtp, obj=self(), prev_state=OldStatus, new_state=NewStatus, ts=timestamp:now_i()}),
			loop({NewStatus, NewConfig});
		M ->
			debug:log("erlmon_smtp:UNKNOWN: ~p", [M]),
			loop(State)
	end.

config(Config) ->
	erlmon_smtp ! {self(), config, Config},
	receive
		{ok, Reply} -> ok
	end,
	Reply.

set_config(State, Config) ->
	case State of
		{_Status, Config} ->
			debug:log("erlmon_smtp: config already set"),
			State;
		_ ->
			debug:log("erlmon_smtp: setting config: ~p", [Config]),
			{ok, NewStatus} = find_primary_smtp(),
			{NewStatus, Config}
	end.

status() ->
	erlmon_smtp ! {self(), status},
	receive
		{status, Reply} -> ok
	end,
	Reply.

find_primary_smtp() ->
	case global:whereis_name(erlmon_smtp) of
		undefined ->
			debug:log("erlmon_smtp: no primary smtp gateway.  becoming primary."),
			case global:register_name(erlmon_smtp, whereis(erlmon_smtp)) of
				yes ->
					debug:log("erlmon_smtp: primary smtp gateway"),
					{ok, primary};
				no ->
					debug:log("erlmon_smtp: failed to become primary gateway"),
					{ok, enabled}
			end;
		Pid ->
			debug:log("erlmon_smtp: primary smtp gateway found: ~p", [Pid]),
			debug:log("erlmon_smtp: becoming secondary"),
			erlang:monitor(process, Pid),
			{ok, secondary}
	end.

