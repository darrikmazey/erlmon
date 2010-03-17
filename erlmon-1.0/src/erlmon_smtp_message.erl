
-module(erlmon_smtp_message).
-author(darrik@darmasoft.com).

-export([create/3]).
-export([test/0]).

-include("include/erlmon.hrl").

create(Config, SC, To) ->
	From = Config#smtp_config.user,
	Subject = create_subject(SC),
	Body = create_body(SC),
	email_msg:simp_msg(From, To, Subject, Body).

create_subject(#state_change{}=SC) ->
	Node = to_list(SC#state_change.node),
	Obj = to_list(SC#state_change.obj),
	ObjType = to_list(SC#state_change.objtype),
	PrevState = to_list(SC#state_change.prev_state),
	NewState = to_list(SC#state_change.new_state),
	lists:flatten(io_lib:format("~s:~s.~s (~s -> ~s)", [Node, ObjType, Obj, PrevState, NewState])).

create_body(#state_change{}=SC) ->
	"test body".

to_list(Arg) when is_atom(Arg) ->
	atom_to_list(Arg);
to_list(Arg) when is_list(Arg) ->
	Arg.

test() ->
	C = #smtp_config{host="mail.darmasoft.com", user="darrik@darmasoft.com", pass="testpass"},
	SC = #state_change{sender=self(), node=node(), objtype=tcp_port, obj="localhost:22", prev_state=up, new_state=down, ts=timestamp:now_i()},
	To = "darrik@darmasoft.com",
	create(C, SC, To).
