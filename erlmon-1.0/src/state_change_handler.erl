
-module(state_change_handler).
-author(darrik@darmasoft.com).

-behaviour(gen_event).

-include("include/erlmon.hrl").

-export([init/1]).
-export([handle_event/2]).
-export([terminate/2]).

init(_) ->
	{ok, []}.

handle_event(#state_change{}=Event, State) ->
	debug:log("state_change_handler:state_change: ~p", [Event]),
	storage:state_change(Event),
	{ok, State};
handle_event(Event, State) ->
	debug:log("state_change_handler:UNKNOWN_EVENT: ~p", [Event]),
	{ok, State}.

terminate(_Args, _State) ->
	ok.
