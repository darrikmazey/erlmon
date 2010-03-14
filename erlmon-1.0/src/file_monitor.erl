
-module(file_monitor).
-author(darrik@darmasoft.com).

-export([start/1]).
-export([init/1]).
-export([monitor/1]).

-include_lib("kernel/include/file.hrl").
-include("include/erlmon.hrl").

monitor(Path) ->
	file_monitor_man:monitor(Path).

start(FileName) ->
	{ok, spawn_link(?MODULE, init, [FileName])}.

init(FileName) ->
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			debug:log("fileinfo: ~p", [FileInfo]),
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unmonitored, new_state=unchanged, ts=timestamp:now_i()}),
			loop(FileName, FileInfo);
		{error, Reason} ->
			debug:log("error: ~p", [Reason])
	end.

loop(FileName, State) ->
	receive
		M ->
			debug:log("file_monitor: ~p: UNKNOWN: ~p", [self(), M])
		after
			3000 ->
	case file:read_file_info(FileName) of
		{ok, State} ->
			NewState = State,
			loop(FileName, NewState);
		State ->
			NewState = State,
			loop(FileName, NewState);
		{ok, FileInfo} ->
			diff(FileName, State, FileInfo),
			NewState = FileInfo,
			loop(FileName, NewState);
		{error, Reason} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=nonexistent, ts=timestamp:now_i()}),
			NewState = {error, Reason},
			loop(FileName, NewState)
	end
	end.

diff(FileName, {error, _Reason}, _NewFileInfo) ->
	state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=nonexistent, new_state=unchanged, ts=timestamp:now_i()}),
	ok;
diff(FileName, OldFileInfo, NewFileInfo) ->
	case {OldFileInfo#file_info.size, NewFileInfo#file_info.size} of
		{_SA, _SA} ->
			same;
		{SA, SB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_size_changed, data={SA, SB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.type, NewFileInfo#file_info.type} of
		{_TA, _TA} ->
			same;
		{TA, TB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_type_changed, data={TA, TB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.access, NewFileInfo#file_info.access} of
		{_AA, _AA} ->
			same;
		{AA, AB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_access_changed, data={AA, AB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.atime, NewFileInfo#file_info.atime} of
		{_ATA, _ATA} ->
			same;
		{ATA, ATB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_atime_changed, data={ATA, ATB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.mtime, NewFileInfo#file_info.mtime} of
		{_MTA, _MTA} ->
			same;
		{MTA, MTB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_mtime_changed, data={MTA, MTB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.ctime, NewFileInfo#file_info.ctime} of
		{_CTA, _CTA} ->
			same;
		{CTA, CTB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_ctime_changed, data={CTA, CTB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.mode, NewFileInfo#file_info.mode} of
		{_MA, _MA} ->
			same;
		{MA, MB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_mode_changed, data={MA, MB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.links, NewFileInfo#file_info.links} of
		{_LA, _LA} ->
			same;
		{LA, LB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_links_changed, data={LA, LB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.major_device, NewFileInfo#file_info.major_device} of
		{_MDA, _MDA} ->
			same;
		{MDA, MDB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_major_device_changed, data={MDA, MDB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.minor_device, NewFileInfo#file_info.minor_device} of
		{_MIDA, _MIDA} ->
			same;
		{MIDA, MIDB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_minor_device_changed, data={MIDA, MIDB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.inode, NewFileInfo#file_info.inode} of
		{_IA, _IA} ->
			same;
		{IA, IB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_inode_changed, data={IA, IB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.uid, NewFileInfo#file_info.uid} of
		{_UIDA, _UIDA} ->
			same;
		{UIDA, UIDB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_uid_changed, data={UIDA, UIDB}, ts=timestamp:now_i()})
	end,
	case {OldFileInfo#file_info.gid, NewFileInfo#file_info.gid} of
		{_GIDA, _GIDA} ->
			same;
		{GIDA, GIDB} ->
			state_change_em:notify(#state_change{sender=self(), node=node(), objtype=file, obj=FileName, prev_state=unchanged, new_state=file_gid_changed, data={GIDA, GIDB}, ts=timestamp:now_i()})
	end.
