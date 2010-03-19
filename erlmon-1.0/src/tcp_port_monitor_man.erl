
-module(tcp_port_monitor_man).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).
-export([monitor/2]).
-export([unmonitor/2]).
-export([status/0]).

-include("include/erlmon.hrl").

start_link() ->
	debug:log("tcp_port_monitor_man: starting"),
	register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
	erlmon:finished(?MODULE),
	{ok, Pid}.

init() ->
	loop([]).

loop(State) ->
	receive
		{Sender, status} ->
			Sender ! {ok, State},
			loop(State);
	  {start, Host, Port} ->
			NewState = start_conditionally(Host, Port, State),
			loop(NewState);
		{stop, Host, Port} ->
			NewState = stop_conditionally(Host, Port, State),
			loop(NewState);
		M ->
			debug:log("tcp_port_monitor_man: UNKNOWN: ~p", [M]),
			loop(State)
	end.

monitor(Host, Port) ->
	?MODULE ! {start, Host, Port},
	ok.

unmonitor(Host, Port) ->
	?MODULE ! {stop, Host, Port},
	ok.

status() ->
	?MODULE ! {self(), status},
	receive
		{ok, Status} -> ok
	end,
	Status.

start_conditionally(Host, Port, []) ->
	debug:log("tcp_port_monitor_man: monitoring ~p:~p", [Host, Port]),
	{ok, Pid} = tcp_port_monitor_sup:monitor(Host, Port),
	[#tcp_port_monitor{host=Host, port=Port, pid=Pid}];
start_conditionally(Host, Port, [#tcp_port_monitor{host=Host, port=Port}|_T]=State) ->
	debug:log("tcp_port_monitor_man: ~p:~p already monitored", [Host, Port]),
	State;
start_conditionally(Host, Port, [H|T]) ->
	[H | start_conditionally(Host, Port, T)].

stop_conditionally(Host, Port, []) ->
	debug:log("tcp_port_monitor_man: not monitoring ~p:~p", [Host, Port]),
	[];
stop_conditionally(Host, Port, [#tcp_port_monitor{host=Host, port=Port, pid=Pid}|T]) ->
	debug:log("tcp_port_monitor_man: no longer monitoring ~p:~p (~p)", [Host, Port, Pid]),
	tcp_port_monitor_sup:unmonitor(Host, Port),
	Pid ! stop,
	T;
stop_conditionally(Host, Port, [H|T]) ->
	[H | stop_conditionally(Host, Port, T)].
