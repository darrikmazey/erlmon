
-module(process_list).
-author(darrik@darmasoft.com).

-include("include/erlmon.hrl").

-define(TIMEOUT, 5000).

-export([
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	count/0,
	get_by_regexp/1,
	get_by_cmd/1,
	get_by_user/1,
	get_by_pid/1,
	list/0,
	refresh/0,
	refresh_timer/1
]).

start_link() ->
	R = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	spawn(process_list, refresh_timer, [?TIMEOUT]),
	R.

init([]) ->
	{ok, []}.

handle_call({count}, _From, State) ->
	Response = length(State),
	{reply, Response, State};
handle_call({get, regexp, CmdRe}, _From, State) ->
	Response = get_processes_by_cmd_re(CmdRe, State),
	{reply, Response, State};
handle_call({get, pid, Pid}, _From, State) ->
	Response = get_processes_by_pid(Pid, State),
	{reply, Response, State};
handle_call({get, user, User}, _From, State) ->
	Response = get_processes_by_user(User, State),
	{reply, Response, State};
handle_call({get, cmd, Cmd}, _From, State) ->
	Response = get_processes_by_cmd(Cmd, State),
	{reply, Response, State};
handle_call({list}, _From, State) ->
	Response = {ok, State},
	{reply, Response, State};
handle_call(_Msg, _From, State) ->
	Response = {error, unknown_call},
	{reply, Response, State}.

handle_cast({refresh}, _State) ->
	NewState = ps:list(),
	{noreply, NewState};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

list() ->
	gen_server:call(?MODULE, {list}).

refresh() ->
	R = gen_server:cast(?MODULE, {refresh}),
	spawn(process_list, refresh_timer, [?TIMEOUT]),
	R.

count() ->
	gen_server:call(?MODULE, {count}).

get_by_pid(Pid) ->
	gen_server:call(?MODULE, {get, pid, Pid}).

get_by_user(User) ->
	gen_server:call(?MODULE, {get, user, User}).

get_by_cmd(Cmd) ->
	gen_server:call(?MODULE, {get, cmd, Cmd}).

get_by_regexp(CmdRe) ->
	gen_server:call(?MODULE, {get, regexp, CmdRe}).


get_processes_by_pid(Pid, State) when is_list(Pid) ->
	PidInt = list_to_integer(Pid),
	get_processes_by_pid(PidInt, State);
get_processes_by_pid(Pid, [#process{pid=Pid}=Process|T]) ->
	[Process|get_processes_by_pid(Pid, T)];
get_processes_by_pid(Pid, [_H|T]) ->
	get_processes_by_pid(Pid, T);
get_processes_by_pid(_Pid, []) ->
	[].

get_processes_by_user(User, [#process{user=User}=Process|T]) ->
	[Process|get_processes_by_user(User, T)];
get_processes_by_user(User, [_H|T]) ->
	get_processes_by_user(User, T);
get_processes_by_user(_User, []) ->
	[].

get_processes_by_cmd(Cmd, [#process{cmd=Cmd}=Process|T]) ->
	[Process|get_processes_by_cmd(Cmd, T)];
get_processes_by_cmd(Cmd, [_H|T]) ->
	get_processes_by_cmd(Cmd, T);
get_processes_by_cmd(_Cmd, []) ->
	[].

get_processes_by_cmd_re(CmdRe, [#process{cmd=Cmd}=Process|T]) ->
	ReRet = re:run(Cmd, CmdRe),
	case ReRet of
		{match, _} -> [Process|get_processes_by_cmd_re(CmdRe, T)];
		nomatch -> get_processes_by_cmd_re(CmdRe, T)
	end;
get_processes_by_cmd_re(CmdRe, [_H|T]) ->
	get_processes_by_cmd_re(CmdRe, T);
get_processes_by_cmd_re(_CmdRe, []) ->
	[].

refresh_timer(T) ->
	receive
	after
		T ->
			refresh()
	end.

