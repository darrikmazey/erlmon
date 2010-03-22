-module(config).
-behaviour(gen_server).
-author(chad@inakanetworks.com).
-include("include/erlmon.hrl").
-include("include/config.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(SERVER, {global, ?MODULE}).
-define(CONFIG_FILE, "config.lua").

-export([
         authenticate/2,
         reload/0,
         setting/1,
         settings/0,
         start_link/0,
         update_file/1 ]).

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

%% config settings can be accessed as:
%% config:setting([smtp,auth,username]) => "bob"

settings() -> 
  gen_server:call(config,erlmon).

setting(Type) when is_atom(Type) -> 
  setting([Type]);

setting(Types) -> 
  Settings = gen_server:call(config,erlmon),
  find_setting(Settings,Types).

find_setting(Settings,[Type|Types]) -> 
  Result = lists:keyfind(atom_to_list(Type),1,Settings),
  case Result of
    false -> 
      Result;
    {_Name,NewSettings} -> find_setting(NewSettings,Types)
  end;

find_setting(Settings,[]) -> 
  Settings.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  debug:log("CONFIG: init~n"),
  {ok,State} = lua:new_state(),
  debug:log("CONFIG: lua state initialized. ~n"),
	erlmon:finished(?MODULE),
  {ok, #config_state{lua_state=State}}.

%% reload config
handle_call({reload,_ReloadType}, _From, _State) ->
  debug:log("CONFIG: loading lua file"),
  {ok, L} = lua:new_state(),
  NewState = #config_state{lua_state=L},
  Reply = lua:dofile(L,?CONFIG_FILE),
  case Reply of
    {error,_} -> 
      debug:log("CONFIG: error in configuration file, NOT reloading.",[]),
      {reply, Reply, _State};
    _ -> 
    Config = lua:gettable(L,global,"Erlmon"),

    {"monitors",Methods} = lists:keyfind("monitors",1,Config),
    {"list",Monitors} = lists:keyfind("list",1,Methods),
    apply_config_list(Monitors),
    debug:log("CONFIG: reloaded"),
    {reply, Reply, NewState}
  end;

handle_call(lua_state, _From, State) -> 
  {reply,State#config_state.lua_state,State};

handle_call(erlmon, _From, State) -> 
  L = State#config_state.lua_state,
  Reply = lua:gettable(L,global,"Erlmon"),
  {reply,Reply,State};

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
  lists:map(fun({Monitor,MonitorList}) -> apply_monitors(Monitor,MonitorList) end,List),
  ok.

apply_monitors(Monitor,MonitorList) ->
  debug:log("CONFIG: Telling monitor ~p to start the following monitors: ~p ",[Monitor,MonitorList]),
	Mod = list_to_atom(Monitor ++ "_monitor"),
	erlang:apply(Mod, monitor, [MonitorList]),
  ok.

%% testcases
config_test() -> ok.
