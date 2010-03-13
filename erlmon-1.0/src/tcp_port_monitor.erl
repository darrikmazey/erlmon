
-module(tcp_port_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([init/1]).
-export([monitor/2]).

-include("include/erlmon.hrl").

monitor(Host, Port) ->
	tcp_port_monitor_man:monitor(Host, Port).

start([Host, Port]) ->
	{ok, spawn_link(?MODULE, init, [Host, Port])}.

init([Host, Port]) ->
	debug:log("tcp_port_monitor: initializing monitor for ~p:~p", [Host, Port]),
	loop([Host, Port], []).

loop([Host, Port], State) ->
	receive
		M ->
			debug:log("tcp_port_monitor: ~p: UNKNOWN: ~p", [self(), M])
		after
			3000 ->
			loop([Host, Port], State)
	end.

