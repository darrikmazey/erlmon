
-module(storage_sup).
-author(darrik@darmasoft.com).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	wait_for_storage(),
	erlmon:finished(?MODULE),
	R.

init(_) ->
	debug:log("storage_sup: starting storage"),
	Child = {storage, {storage, start, []}, permanent, 2000, worker, [storage]},
	{ok, {{one_for_one, 1, 1}, [Child]}}.

wait_for_storage() ->
	storage:status().
