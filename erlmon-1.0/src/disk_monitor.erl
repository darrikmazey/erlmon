
-module(disk_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([start_all/0]).
-export([init/1]).
-export([monitor/1]).

-include("include/erlmon.hrl").

monitor(Path) ->
	disk_monitor_sup:monitor(Path).

start_all() ->
	start_each(df:list()).

start_each([#filesystem{path=Path}|T]) ->
	start(Path),
	start_each(T);
start_each([]) -> ok.

start(Path) ->
	{ok, spawn_link(?MODULE, init, [Path])}.

init(Path) ->
	debug:log("disk_monitor:init(~p)", [Path]),
	Filesystem = df:find_path(Path),
	case Filesystem of
		not_found ->
			null;
		#filesystem{} ->
			loop(first, Filesystem)
	end.

loop(OldState, Filesystem) ->
	receive
		stop ->
			stopped
	after
		3000 ->
			#filesystem{path=Path, percent=_Percent} = Filesystem,
			NewFilesystem = df:find_path(Path),
			#filesystem{path=Path, percent=NewPercent, mount=Mount} = NewFilesystem,
			ObjName = lists:flatten([Path, ":"| Mount]),
			case NewPercent > 90 of
				false ->
					case OldState of
						alert ->
							state_change_em:notify(#state_change{sender=self(), node=node(), objtype=disk, obj=ObjName, prev_state=alert, new_state=ok, data=NewPercent, ts=timestamp:now_i()}),
							loop(ok, NewFilesystem);
						first ->
							state_change_em:notify(#state_change{sender=self(), node=node(), objtype=disk, obj=ObjName, prev_state=none, new_state=ok, data=NewPercent, ts=timestamp:now_i()}),
							loop(ok, NewFilesystem);
						_ ->
							loop(ok, NewFilesystem)
					end;
				true ->
					case OldState of
						ok ->
							state_change_em:notify(#state_change{sender=self(), node=node(), objtype=disk, obj=ObjName, prev_state=ok, new_state=alert, data=NewPercent, ts=timestamp:now_i()}),
							loop(alert, NewFilesystem);
						first ->
							state_change_em:notify(#state_change{sender=self(), node=node(), objtype=disk, obj=ObjName, prev_state=none, new_state=alert, data=NewPercent, ts=timestamp:now_i()}),
							loop(alert, NewFilesystem);
						_ ->
							loop(alert, NewFilesystem)
					end
			end
	end.
