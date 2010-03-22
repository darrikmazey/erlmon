
-module(alert_handler).
-author(darrik@darmasoft.com).

-behaviour(gen_event).

-include("include/erlmon.hrl").

-export([init/1]).
-export([code_change/3]).
-export([handle_state_change/2]).
-export([handle_event/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([test/0]).
-export([register_alert_filter/1]).
-export([unregister_alert_filter/1]).
-export([match_all_filter/1]).

init(_) ->
	{ok, []}.

handle_event(#state_change{}=Event, State) ->
	debug:log("alert_handler:state_change: ~p", [Event]),
	dispatch_state_change(Event, State),
	{ok, State};
handle_event({add_alert_filter, #alert_filter{}=AF}, State) ->
	debug:log("alert_handler:add_alert_filter: ~p", [AF]),
	NewState = add_filter(AF, State),
	{ok, NewState};
handle_event({remove_alert_filter, #alert_filter{}=AF}, State) ->
	debug:log("alert_handler:remove_alert_filter: ~p", [AF]),
	NewState = remove_filter(AF, State),
	{ok, NewState};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Args, _State) ->
	ok.

add_filter(AF, [AF|_T]=State) ->
	debug:log("alert_handler: alert_filter already exists"),
	State;
add_filter(AF, [H|T]) ->
	[H|add_filter(AF, T)];
add_filter(AF, []) ->
	debug:log("alert_handler: alert_filter added"),
	[AF].

remove_filter(AF, [AF|T]) ->
	debug:log("alert_handler: alert_filter removed"),
	T;
remove_filter(AF, [H|T]) ->
	[H|remove_filter(AF, T)];
remove_filter(_AF, []) ->
	debug:log("alert_handler: alert_filter does not exist"),
	[].

register_alert_filter(#alert_filter{}=AF) ->
	state_change_em:notify({add_alert_filter, AF}).

unregister_alert_filter(#alert_filter{}=AF) ->
	state_change_em:notify({remove_alert_filter, AF}).

test() ->
	SC = #state_change{sender=nil, node=nil, objtype=file, obj=nil, prev_state=nil, new_state=file_ctime_changed, ts=nil},
	SF = #state_change_filter{state_change=SC, fields=[objtype, new_state]},
	#alert_filter{scf=SF, to="darrik@darmasoft.com"}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

dispatch_state_change(#state_change{}=SC, State) ->
	debug:log("alert_handler: handling state_change for ~p:~p", [SC#state_change.objtype, SC#state_change.obj]),
	spawn(?MODULE, handle_state_change, [SC, State]),
	ok.

handle_state_change(SC, [#alert_filter{scf=#state_change_filter{}=SCF, to=ToAddress}|T]) ->
	case state_change:match(SC, SCF) of
		true ->
			debug:log("alert_handler: ~p:~p (~p -> ~p) matched filter!", [SC#state_change.objtype, SC#state_change.obj, SC#state_change.prev_state, SC#state_change.new_state]),
			erlmon_smtp:alert(SC, ToAddress),
			alert_sent;
		false ->
			handle_state_change(SC, T)
	end;
handle_state_change(SC, [_H|T]) ->
	handle_state_change(SC, T);
handle_state_change(SC, []) ->
	debug:log("alert_handler: ~p:~p (~p -> ~p) did not match any filters", [SC#state_change.objtype, SC#state_change.obj, SC#state_change.prev_state, SC#state_change.new_state]),
	no_alert.

match_all_filter(ToAddress) ->
	SC = #state_change{sender=nil, node=nil, objtype=nil, obj=nil, prev_state=nil, new_state=nil, ts=nil},
	SCF = #state_change_filter{state_change=SC, fields=[]},
	#alert_filter{scf=SCF, to=ToAddress}.
	
