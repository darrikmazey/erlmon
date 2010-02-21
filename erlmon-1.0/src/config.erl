-module(config).
-behaviour(gen_server).
-author(chad@inakanetworks.com).
-include("include/erlmon.hrl").
-include("include/config.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(SERVER, {global, ?MODULE}).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% The config system is the glue between the lua configuration
%% code and the Erlang system. The goal of the system is for 
%% each piece to do as little as possible. In this case, the config
%% system simply starts a special case file_monitor that watches the
%% erlmon config file. When a change is detected the file is reloaded.

%% TODO: Darrik this needs to pass a callback function, 
%% i want to call config:handle_info/2 if the file changes
%% file_monitor:start("erlmon.conf"),
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #config_state{}}.

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

%% testcases
config_test() -> ok.
