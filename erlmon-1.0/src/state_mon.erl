
-module(state_mon).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-include("include/erlmon.hrl").

start() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("state_mon: initializing"),
	loop([]).

loop(State) ->
	receive
		Msg = #state_change{sender=_Sender, node=_Node, objtype=ObjType, obj=Obj, prev_state=PrevState, new_state=NewState, ts=_TS} ->
			debug:log("~p:~p changed: ~p -> ~p", [ObjType, Obj, PrevState, NewState]),
			storage:state_change(Msg),
			loop(State);
		M ->
			debug:log("state_mon:UNKNOWN: ~p", [M]),
			loop(State)
	end.
