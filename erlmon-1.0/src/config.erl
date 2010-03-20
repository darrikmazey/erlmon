-module(config).
-behaviour(gen_server).
-author(chad@inakanetworks.com).
-include("include/erlmon.hrl").
-include("include/config.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(SERVER, {global, ?MODULE}).
-define(CONFIG_FILE, "config.lua").

-export([start_link/0,
         update_file/1,
         reload/0,
         authenticate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% The config system is the glue between the lua configuration
%% code and the Erlang system. The goal of the system is for 
%% each piece to do as little as possible. In this case, the config
%% system simply starts a special case file_monitor that watches the
%% erlmon config file. When a change is detected the file is reloaded.

reload() -> 
  debug:log("CONFIG:reload"),
  gen_server:call(config, {reload,event}).

update_file(Content) -> 
  file:write_file(?CONFIG_FILE,Content).

authenticate(Login,Password) -> 
  debug:log("CONFIG:authenticating"),
  gen_server:call(config, {authenticate, Login, Password}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  debug:log("CONFIG: init~n"),
  {ok,State} = lua:new_state(),
  debug:log("CONFIG: lua state initialized. ~n"),
	erlmon:finished(?MODULE),
  {ok, #config_state{lua_state=State}}.

%% reload config
handle_call({reload,_ReloadType}, _From, State) ->
  debug:log("CONFIG: loading lua file"),
  L = State#config_state.lua_state,
  Reply = lua:dofile(L,?CONFIG_FILE),
  Config = lua:gettable(L,global,"Erlmon"),

  {"monitors",Methods} = lists:keyfind("monitors",1,Config),
  {"list",Monitors} = lists:keyfind("list",1,Methods),
  debug:log("CONFIG: Monitors: ~p", [Monitors]),
  %%List = [
  %%  ["tcp_port",["localhost",22]],
  %%  ["process",["/usr/sbin/sshd"]]
  %%],
  apply_config_list(Monitors),
  debug:log("CONFIG: reloaded"),
  {reply, Reply, State};

%% call lua authenticate method
handle_call({authenticate, Login, Password}, _From, State) ->
  L = State#config_state.lua_state,
  lua:getfield(L, global, "authenticate"),
  lua:pushstring(L, Login),
  lua:pushstring(L, Password),
  lua:call(L, 2, 1),
  {ok,boolean,Reply} = lua:pop(L),
  debug:log("CONFIG: Authenticate: Lua returned: ~p",[Reply]),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

apply_config_list(List) -> 
  lists:map(fun(Monitor) -> apply_monitors(Monitor) end,List),
  ok.

apply_monitors(Monitor) ->
  debug:log("Monitor ~p starting.",[Monitor]),
	%%[MonType, Args] = Monitor,
	%%Mod = list_to_atom(MonType ++ "_monitor"),
	%%erlang:apply(Mod, monitor, Args),
  ok.

%% testcases
config_test() -> ok.
