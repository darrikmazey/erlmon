
-module(process_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([init/1]).

-include("include/erlmon.hrl").

start(ProcessName) ->
	spawn_link(?MODULE, init, [ProcessName]).

init(ProcessName) ->
	debug:log("process_monitor: starting for ~p", [ProcessName]),
	Pid = ps:pid_for_process(ProcessName),
	case Pid of
		null ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=unknown, new_state=not_running, ts=timestamp:now_i()},
			loop(null, ProcessName, not_running);
		P ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=unknown, new_state=running, ts=timestamp:now_i()},
			loop(P, ProcessName, running)
	end.

loop(Pid, ProcessName, running) ->
	timer:sleep(3000),
	Cpid = ps:pid_for_process(ProcessName),
	case Cpid of
		null ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=running, new_state=not_running, ts=timestamp:now_i()},
			loop(null, ProcessName, not_running);
		Pid ->
			loop(Pid, ProcessName, running);
		NewPid ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=running, new_state=restarted, ts=timestamp:now_i()},
			loop(NewPid, ProcessName, restarted)
	end;
loop(null, ProcessName, not_running) ->
	timer:sleep(3000),
	Cpid = ps:pid_for_process(ProcessName),
	case Cpid of
		null ->
			loop(null, ProcessName, not_running);
		Pid ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=not_running, new_state=running, ts=timestamp:now_i()},
			loop(Pid, ProcessName, running)
	end;
loop(Pid, ProcessName, restarted) ->
	timer:sleep(3000),
	Cpid = ps:pid_for_process(ProcessName),
	case Cpid of
		null ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=not_running, ts=timestamp:now_i()},
			loop(null, ProcessName, not_running);
		Pid ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=running, ts=timestamp:now_i()},
			loop(Pid, ProcessName, running);
		NewPid ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=process, obj=ProcessName, prev_state=restarted, new_state=restarted, ts=timestamp:now_i()},
			loop(NewPid, ProcessName, restarted)
	end.
