
-module(file_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([init/1]).

-include_lib("kernel/include/file.hrl").
-include("include/erlmon.hrl").

start(FileName) ->
	spawn_link(?MODULE, init, [FileName]).

init(FileName) ->
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			debug:log("fileinfo: ~p", [FileInfo]),
			state_mon ! #state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unmonitored, new_state=unchanged, ts=timestamp:now_i()},
			loop(FileName, FileInfo);
		{error, Reason} ->
			debug:log("error: ~p", [Reason])
	end.

loop(FileName, State) ->
	timer:sleep(3000),
	case file:read_file_info(FileName) of
		{ok, State} ->
			NewState = State;
		{ok, FileInfo} ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=changed, ts=timestamp:now_i()},
			NewState = FileInfo;
		{error, Reason} ->
			state_mon ! #state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=nonexistent, ts=timestamp:now_i()},
			NewState = {error, Reason}
	end,
	loop(FileName, NewState).

