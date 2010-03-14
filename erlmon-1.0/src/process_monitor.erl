
-module(process_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([init/1]).
-export([monitor/1]).
-export([unmonitor/1]).

-include("include/erlmon.hrl").

monitor(ProcessName) ->
	process_monitor_man:monitor(ProcessName).

unmonitor(ProcessName) ->
	process_monitor_man:unmonitor(ProcessName).

start(ProcessName) ->
	{ok, spawn_link(?MODULE, init, [ProcessName])}.

init(Pid) when is_integer(Pid) ->
	debug:log("process_monitor: starting for ~p", [Pid]),
	Name = ps:name_for_pid(Pid),
	case Name of
		null ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=Pid, prev_state=unknown, new_state=not_running, ts=timestamp:now_i()});
		P ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=P, prev_state=unknown, new_state=running, ts=timestamp:now_i()}),
			loop(lists:flatten(io_lib:format("~p", [Pid])), P, running)
	end;
init(ProcessName) ->
	debug:log("process_monitor: starting for ~p", [ProcessName]),
	Pid = ps:pid_for_process(ProcessName),
	case Pid of
		null ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=unknown, new_state=not_running, ts=timestamp:now_i()}),
			loop(null, ProcessName, not_running);
		P ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=unknown, new_state=running, ts=timestamp:now_i()}),
			loop(P, ProcessName, running)
	end.

loop(Pid, ProcessName, running) ->
	receive
		stop ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=running, new_state=unmonitored, ts=timestamp:now_i()})
	after
		3000 ->
			Cpid = ps:pid_for_process(ProcessName),
			case Cpid of
				null ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=running, new_state=not_running, ts=timestamp:now_i()}),
					loop(null, ProcessName, not_running);
				Pid ->
					loop(Pid, ProcessName, running);
				NewPid ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=running, new_state=restarted, ts=timestamp:now_i()}),
					loop(NewPid, ProcessName, restarted)
			end
	end;
loop(null, ProcessName, not_running) ->
	receive
		stop ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=not_running, new_state=unmonitored, ts=timestamp:now_i()})
	after
		3000 ->
			Cpid = ps:pid_for_process(ProcessName),
			case Cpid of
				null ->
					loop(null, ProcessName, not_running);
				Pid ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=not_running, new_state=running, ts=timestamp:now_i()}),
					loop(Pid, ProcessName, running)
			end
	end;
loop(Pid, ProcessName, restarted) ->
	receive
		stop ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=unmonitored, ts=timestamp:now_i()})
	after
		3000 ->
			Cpid = ps:pid_for_process(ProcessName),
			case Cpid of
				null ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=not_running, ts=timestamp:now_i()}),
					loop(null, ProcessName, not_running);
				Pid ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=running, ts=timestamp:now_i()}),
					loop(Pid, ProcessName, running);
				NewPid ->
					state_change_em:notify(#state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=restarted, ts=timestamp:now_i()}),
					loop(NewPid, ProcessName, restarted)
			end
	end.
