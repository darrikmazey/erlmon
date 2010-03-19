
-module(process_list_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	erlmon:finished(?MODULE),
	R.

init(_) ->
	debug:log("process_list_sup: starting process_list"),
	Child = {process_list, {process_list, start_link, []}, permanent, 2000, worker, [process_list]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.
