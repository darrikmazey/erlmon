
-module(config_file_change_handler).
-author(darrik@darmasoft.com).

-behaviour(gen_event).

-include("include/erlmon.hrl").

-export([init/1]).
-export([handle_event/2]).
-export([terminate/2]).

init(_) ->
	{ok, []}.

handle_event(#state_change{objtype=file, obj="erlmon.cfg", new_state=changed}=Event, State) ->
	debug:log("CONFIG FILE CHANGED: ~p", [Event]),
	{ok, State};
handle_event(Event, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.
