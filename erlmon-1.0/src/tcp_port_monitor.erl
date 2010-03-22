
-module(tcp_port_monitor).
-author(darrik@darmasoft.com).

-export([start/2]).
-export([init/1]).
-export([monitor/1,monitor/2]).
-export([unmonitor/2]).
-export([status/0]).
-export([status/1]).

-include("include/erlmon.hrl").

status() ->
	tcp_port_monitor_man:status().

status(Pid) ->
	Pid ! {self(), status},
	receive
		{ok, Status} -> ok
	end,
	{ok, Status}.

%% Config sends us, from Lua, something like:
%% monitor([{"memcached",[{"host","localhost"},{"port",11211}]}]})
monitor([]) ->
	[];
monitor([Description|T]) -> 
  {_Name,Args} = Description,
  {"host",Host} = lists:keyfind("host",1,Args),
  {"port",Port} = lists:keyfind("port",1,Args),
  monitor(Host,Port),
	monitor(T).

monitor(Host, Port) ->
	tcp_port_monitor_man:monitor(Host, Port).

unmonitor(Host, Port) ->
	tcp_port_monitor_man:unmonitor(Host, Port).

start(Host, Port) ->
	debug:log("tcp_port_monitor:start([~p, ~p])", [Host, Port]),
	{ok, spawn_link(?MODULE, init, [[Host, Port]])}.

init([Host, Port]) ->
	debug:log("tcp_port_monitor: initializing monitor for ~p:~p", [Host, Port]),
	loop([Host, Port], unmonitored).

loop([Host, Port], State) ->
	ObjName = lists:concat(io_lib:format("~s~s~p", [Host, ":",Port])),
	receive
		stop ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=tcp_port, obj=ObjName, prev_state=State, new_state=unmonitored, ts=timestamp:now_i()});
		{Sender, status} ->
			Sender ! {ok, State},
			loop([Host, Port], State);
		M ->
			debug:log("tcp_port_monitor: ~p: UNKNOWN: ~p", [self(), M])
		after
			3000 ->
				case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
					{ok, Sock} ->
						gen_tcp:close(Sock),
						case State of
							up ->
								NewState = up,
								loop([Host, Port], NewState);
							OldState ->
								state_change_em:notify(#state_change{sender=self(), node=node(), objtype=tcp_port, obj=ObjName, prev_state=OldState, new_state=up, ts=timestamp:now_i()}),
								NewState = up,
								loop([Host, Port], NewState)
						end;
					{error, _Reason} ->
					  case State of
							down ->
								NewState = down,
								loop([Host, Port], NewState);
							OldState ->
								state_change_em:notify(#state_change{sender=self(), node=node(), objtype=tcp_port, obj=ObjName, prev_state=OldState, new_state=down, ts=timestamp:now_i()}),
								NewState = down,
								loop([Host, Port], NewState)
						end
				end
	end.

