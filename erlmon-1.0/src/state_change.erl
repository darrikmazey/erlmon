
-module(state_change).
-author(darrik@darmasoft.com).

-include("include/erlmon.hrl").

-export([match/3]).
-export([match/2]).
-export([collect_booleans/1]).
-export([and_booleans/1]).
-export([match_field/3]).
-export([test_same/0, test_diff/0, test_filter/0]).

match(#state_change{}=SCA, #state_change_filter{state_change=SCB, fields=Fields}) ->
	match(SCA, SCB, Fields).

match(#state_change{}=SCA, #state_change{}=SCB, Fields) when is_list(Fields) ->
	and_booleans(match_fields(SCA, SCB, Fields)).

match_fields(#state_change{}=SCA, #state_change{}=SCB, [Field|T]=Fields) when is_list(Fields) ->
	[match_field(SCA, SCB, Field)|match(SCA, SCB, T)];
match_fields(#state_change{}, #state_change{}, []) ->
	[].
	

match_field(#state_change{}=SCA, #state_change{}=SCB, sender) ->
	case {SCA#state_change.sender, SCB#state_change.sender} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, node) ->
	case {SCA#state_change.node, SCB#state_change.node} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, objtype) ->
	case {SCA#state_change.objtype, SCB#state_change.objtype} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, obj) ->
	case {SCA#state_change.obj, SCB#state_change.obj} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, prev_state) ->
	case {SCA#state_change.prev_state, SCB#state_change.prev_state} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, new_state) ->
	case {SCA#state_change.new_state, SCB#state_change.new_state} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, data) ->
	case {SCA#state_change.data, SCB#state_change.data} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}=SCA, #state_change{}=SCB, ts) ->
	case {SCA#state_change.ts, SCB#state_change.ts} of
		{_A, _A} ->
			true;
		{_A, _B} ->
			false
	end;
match_field(#state_change{}, #state_change{}, _Field) ->
	true.


collect_booleans([true, true|T]) ->
	collect_booleans([true|T]);
collect_booleans([false, false|T]) ->
	collect_booleans([false|T]);
collect_booleans([true, false|T]) ->
	[true|collect_booleans([false|T])];
collect_booleans([false, true|T]) ->
	[false|collect_booleans([true|T])];
collect_booleans([true]) ->
	[true];
collect_booleans([false]) ->
	[false];
collect_booleans([]) ->
	[].

and_booleans([true|T]) ->
	and_booleans(T);
and_booleans([false|_T]) ->
	false;
and_booleans(true) ->
	true;
and_booleans(false) ->
	false;
and_booleans([]) ->
	true.

test_same() ->
	SCA = #state_change{sender=self(), node=node(), objtype=file, obj="config.lua", prev_state=unchanged, new_state=file_ctime_changed, ts=timestamp:now_i()},
	SCB = #state_change{sender=self(), node=node(), objtype=file, obj="config.lua", prev_state=unchanged, new_state=file_ctime_changed, ts=timestamp:now_i()},
	[SCA, SCB].

test_diff() ->
	SCA = #state_change{sender=self(), node=node(), objtype=tcp_port, obj="localhost:22", prev_state=down, new_state=up, ts=timestamp:now_i()},
	SCB = #state_change{sender=self(), node=node(), objtype=tcp_port, obj="localhost:22", prev_state=up, new_state=down, ts=timestamp:now_i()},
	[SCA, SCB].

test_filter() ->
	SCA = #state_change{sender=self(), node=node(), objtype=tcp_port, obj="localhost:22", prev_state=down, new_state=up, ts=timestamp:now_i()},
	SCB = #state_change{sender=self(), node=node(), objtype=tcp_port, obj="localhost:22", prev_state=up, new_state=down, ts=timestamp:now_i()},
	Fields = [objtype, obj],
	SCF = #state_change_filter{state_change=SCB, fields=Fields},
	[SCA, SCF].
