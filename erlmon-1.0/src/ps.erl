
-module(ps).
-author(darrik@darmsoft.com).

-export([list/0]).
-export([list/1]).
-export([dump/1]).
-export([find/1]).
-export([pid_for_process/1]).
-export([name_for_pid/1]).

-include("include/erlmon.hrl").

list() ->
	list([]).

list([]) ->
	Output = os:cmd("ps aux"),
	process(Output).

process(Output) when is_list(Output) ->
	Lines = re:split(Output, "\n", [{return, list}]),
	[_Header|T] = Lines,
	create_records(T).

create_records([H|T]) ->
	[create_record(H) | create_records(T)];
create_records([]) ->
	[].

create_record([Head|[]]) -> create_record(Head);
create_record([]) -> [];
create_record(Line) ->
	Fields = re:split(Line, "\s+", [{return, list}]),
	[User, Pid, Cpu, Mem, Vsz, Rss, Tty, Stat, Start, Time | Cmd] = Fields,
	Rec = #process{user=User, pid=Pid, cpu=Cpu, mem=Mem, vsz=Vsz, rss=Rss, tty=Tty, stat=Stat, start=Start, time=Time, cmd=collapse(Cmd)},
	Rec.

dump([Process|T]) ->
	debug:log("~p", [Process]),
	dump(T).

collapse([]) -> "";
collapse([I]) -> I;
collapse([H|T]) ->
	H ++ " " ++ collapse(T).

find(Pname) ->
	find(Pname, list()).

find(Pname, [H = #process{cmd=Pname}|_T]) -> H;
find(Pname, [_H|T]) ->
	find(Pname, T);
find(_Pname, []) ->
	not_found.

find_regex(Pregex) ->
	find_regex(Pregex, list()).

find_regex(Pregex, [H|T]) ->
  #process{cmd=Pname} = H,
	REReturn = re:run(Pname, Pregex),
	case REReturn of
		{match, _} -> H;
		nomatch -> find_regex(Pregex, T)
	end;
find_regex(_Pregex, []) ->
	not_found.

find_pid(Pid) ->
	find_pid(Pid, list()).

find_pid(Pid, [H = #process{pid=Pid}|_T]) -> H;
find_pid(Pid, [_H|T]) -> find_pid(Pid, T);
find_pid(_Pid, []) -> not_found.

pid_for_process(Pname) ->
	Process = find(Pname),
	case Process of
		not_found -> pid_for_process_regex(Pname);
		P ->
			#process{pid=Pid} = P,
			Pid
	end.

pid_for_process_regex(Pregex) ->
	Process = find_regex(Pregex),
	case Process of
		not_found -> null;
		P ->
			#process{pid=Pid} = P,
			Pid
	end.

name_for_pid(Pid) when is_integer(Pid) ->
	Process = find_pid(lists:flatten(io_lib:format("~p", [Pid]))),
	case Process of
		not_found -> null;
		P ->
			#process{cmd=Cmd} = P,
			Cmd
	end;
name_for_pid(Pid) ->
	Process = find_pid(Pid),
	case Process of
		not_found -> null;
		P ->
			#process{cmd=Cmd} = P,
			Cmd
	end.
