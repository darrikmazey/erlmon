
-module(state_change_em).
-author(darrik@darmasoft.com).

-export([start_link/0]).
-export([add_handler/1]).
-export([add_handler/2]).
-export([notify/1]).
-export([stop/0]).

-define(SERVER, ?MODULE).

start_link() ->
	debug:log("state_change_em: starting"),
	R = gen_event:start_link({local, ?SERVER}),
	erlmon:finished(?MODULE),
	R.

add_handler(Module) ->
	gen_event:add_handler(?SERVER, Module, []).

add_handler(Module, State) ->
	gen_event:add_handler(?SERVER, Module, State).

notify(Event) ->
	gen_event:notify(?SERVER, Event).

stop() ->
	debug:log("state_change_em: stopping"),
	gen_event:stop(?SERVER).

