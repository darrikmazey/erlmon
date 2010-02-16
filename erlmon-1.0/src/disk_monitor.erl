
-module(disk_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([start_all/0]).
-export([init/1]).

-include("include/erlmon.hrl").

start_all() ->
	start_each(df:list()).

start_each([#filesystem{path=Path}|T]) ->
	start(Path),
	start_each(T);
start_each([]) -> ok.

start(Path) ->
	spawn_link(?MODULE, init, [Path]).

init(Path) ->
	Filesystem = df:find_path(Path),
	case Filesystem of
		not_found ->
			null;
		#filesystem{} ->
			loop(ok, Filesystem)
	end.

loop(OldState, Filesystem) ->
	receive
		stop ->
			stopped
	after
		3000 ->
			#filesystem{path=Path, percent=Percent} = Filesystem,
			NewFilesystem = df:find_path(Path),
			#filesystem{path=Path, percent=NewPercent} = NewFilesystem,
			case NewPercent > 90 of
				false ->
					case OldState of
						alert ->
							state_mon ! #state_change{sender=self(), node=node(), objtype=disk, obj=Path, prev_state=alert, new_state=ok, ts=timestamp:now_i()},
							loop(ok, NewFilesystem);
						_ ->
							loop(ok, NewFilesystem)
					end;
				true ->
					case OldState of
						ok ->
							state_mon ! #state_change{sender=self(), node=node(), objtype=disk, obj=Path, prev_state=ok, new_state=alert, ts=timestamp:now_i()},
							loop(alert, NewFilesystem);
						_ ->
							loop(alert, NewFilesystem)
					end
			end
	end.
