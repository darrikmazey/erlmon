
-module(file_monitor_man).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([init/0]).
-export([monitor/1]).
-export([unmonitor/1]).


start_link() ->
	debug:log("file_monitor_man: starting"),
	register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
	erlmon:finished(?MODULE),
	{ok, Pid}.

init() ->
	loop([]).

loop(State) ->
	receive
	  {start, Path} ->
			NewState = start_conditionally(Path, State),
			loop(NewState);
		{stop, Path} ->
			NewState = stop_conditionally(Path, State),
			loop(NewState);
		M ->
			debug:log("file_monitor_man: UNKNOWN: ~p", [M]),
			loop(State)
	end.

monitor(Path) ->
	wait_for_manager(),
	?MODULE ! {start, Path},
	ok.

unmonitor(Path) ->
	?MODULE ! {stop, Path},
	ok.

start_conditionally(Path, []) ->
	debug:log("file_monitor_man: monitoring ~p", [Path]),
	file_monitor_sup:monitor(Path),
	[Path];
start_conditionally(Path, [Path|T]) ->
	debug:log("file_monitor_man: ~p already monitored", [Path]),
	[Path|T];
start_conditionally(Path, [H|T]) ->
	[H | start_conditionally(Path, T)].

stop_conditionally(Path, []) ->
	debug:log("file_monitor_man: not monitoring ~p", [Path]),
	[];
stop_conditionally(Path, [Path|T]) ->
	debug:log("file_monitor_man: no longer monitoring ~p", [Path]),
	file_monitor_sup:unmonitor(Path),
	T;
stop_conditionally(Path, [H|T]) ->
	[H | stop_conditionally(Path, T)].

wait_for_manager() ->
	case whereis(file_monitor_man) of
		undefined ->
			wait_for_manager();
		_ ->
			ok
	end.

