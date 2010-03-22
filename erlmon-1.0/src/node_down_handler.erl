
-module(node_down_handler).
-author(darrik@darmasoft.com).

-behaviour(gen_event).

-include("include/erlmon.hrl").

-export([init/1]).
-export([handle_event/2]).
-export([terminate/2]).

init(_) ->
	{ok, []}.

handle_event(#state_change{objtype=node, new_state=down}=Event, State) ->
	debug:log("node_down_handler: ~p", [Event]),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.
