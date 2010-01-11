
-module(state_mon).
-author(darrik@darmasoft.com).

-export([start/0]).
-export([init/0]).

-include("include/state_mon.hrl").

start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	{ok, Pid}.

init() ->
	debug:log("state_mon: initializing"),
	loop([]).

loop(State) ->
	receive
		#state_change{sender=_Sender, objtype=ObjType, obj=Obj, prev_state=PrevState, new_state=NewState} ->
			debug:log("~p:~p changed: ~p -> ~p", [ObjType, Obj, PrevState, NewState]),
			loop(State);
		M ->
			debug:log("UNKNOWN: ~p", [M]),
			loop(State)
	end.
