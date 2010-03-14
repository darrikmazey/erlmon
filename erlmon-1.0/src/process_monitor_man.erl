
-module(process_monitor_man).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).
-export([monitor/1]).
-export([unmonitor/1]).

-include("include/erlmon.hrl").

start_link() ->
	debug:log("process_monitor_man: starting"),
	register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	loop([]).

loop(State) ->
	receive
	  {start, ProcessName} ->
			NewState = [State|start_conditionally(ProcessName, State)],
			loop(NewState);
		{stop, ProcessName} ->
			NewState = stop_conditionally(ProcessName, State),
			loop(NewState);
		M ->
			debug:log("process_monitor_man: UNKNOWN: ~p", [M]),
			loop(State)
	end.

monitor(ProcessName) ->
	?MODULE ! {start, ProcessName},
	ok.

unmonitor(ProcessName) ->
	?MODULE ! {stop, ProcessName},
	ok.

start_conditionally(ProcessName, []) ->
	debug:log("process_monitor_man: monitoring ~p", [ProcessName]),
	{ok, Pid} = process_monitor_sup:monitor(ProcessName),
	[#process_monitor{name=ProcessName, pid=Pid}];
start_conditionally(ProcessName, [H=#process_monitor{name=ProcessName, pid=Pid}|T]) ->
	debug:log("process_monitor_man: ~p already monitored (~p)", [ProcessName, Pid]),
	[H|T];
start_conditionally(ProcessName, [H|T]) ->
	[H | start_conditionally(ProcessName, T)].

stop_conditionally(ProcessName, []) ->
	debug:log("process_monitor_man: not monitoring ~p", [ProcessName]),
	[];
stop_conditionally(ProcessName, [#process_monitor{name=ProcessName, pid=Pid}|T]) ->
	debug:log("process_monitor_man: no longer monitoring ~p (~p)", [ProcessName, Pid]),
	process_monitor_sup:unmonitor(ProcessName),
	Pid ! stop,
	T;
stop_conditionally(ProcessName, [H|T]) ->
	[H | stop_conditionally(ProcessName, T)].
