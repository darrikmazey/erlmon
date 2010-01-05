
-module(debug).
-author(darrik@darmasoft.com).

-export([log/1]).
-export([log/2]).

-export([log_to/2]).
-export([no_log_to/2]).

-export([init/0]).

-include_lib("include/debug.hrl").

log(Format) ->
	log(Format, []).

log(Format, Args) ->
	start(),
	debug_srv ! #debug_log_msg{sender=self(), format=Format, args=Args},
	true.

log_to(Mod, Options) ->
	start(),
	ModString = atom_to_list(Mod),
	NewModString = lists:flatten(["debug_"|ModString]),
	NewMod = list_to_atom(NewModString),
	debug_srv ! #debug_log_add_method{sender=self(), module=NewMod, options=Options},
	true.

no_log_to(Mod, Options) ->
	start(),
	debug_srv ! #debug_log_rem_method{sender=self(), module=Mod, options=Options},
	true.

start() ->
	case whereis(debug_srv) of
		undefined ->
			register(debug_srv, spawn(debug, init, []));
		_Pid -> true
	end,
	true.

init() ->
	loop([]).

loop(State) ->
	receive
		#debug_log_msg{sender=_Sender, format=Format, args=Args} ->
			dispatch("[~s] " ++ Format, [timestamp:now()|Args], State),
			loop(State);
		#debug_log_add_method{sender=_Sender, module=Mod, options=Options} ->
			NewState = add_to_state({Mod, Options}, State),
			loop(NewState);
		#debug_log_rem_method{sender=_Sender, module=Mod, options=Options} ->
			NewState = remove_from_state({Mod, Options}, State),
			loop(NewState)
	end.

add_to_state(Tuple, [Tuple|T]) -> [Tuple|T];
add_to_state(Tuple, [H|T]) -> [H|add_to_state(Tuple, T)];
add_to_state(Tuple, []) -> [Tuple|[]].

remove_from_state(Tuple, [Tuple|T]) -> T;
remove_from_state(Tuple, [H|T]) -> [H|remove_from_state(Tuple, T)];
remove_from_state(_Tuple, []) -> [].

dispatch(Format, Args, [{Mod, Options}|T]) ->
	apply(Mod, log, [Format, Args, Options]),
	dispatch(Format, Args, T);
dispatch(_Format, _Args, []) -> true.

