-module(lua).

-export([new_state/0,
         close/1,
         call/3,
         concat/2,
	 dostring/2,
	 dofile/2,
	 dump_table/2,
         getfield/3,
         getglobal/2,
				 gettable/3,
				 gettable/2,
         gettop/1,
				 next/2,
				 pop/1,
				 push/2,
         pushboolean/2,
         pushinteger/2,
         pushstring/2,
         pushnil/1,
         pushnumber/2,
         remove/2,
         setfield/3,
         setglobal/2,
         toboolean/2,
         tointeger/2,
         tolstring/2,
         tonumber/2,
         type/2,
				 type_atom/2
				 ]).

-include("lua.hrl").
-include("lua_api.hrl").

new_state() ->
    {ok, lua_driver:open()}.
    
close(L) ->
    lua_driver:close(L).

dostring(#lua{port=Port}, Code) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOSTRING, Code})),
    receive_simple_response().

dofile(#lua{port=Port}, Filename) ->
	port_command(Port, term_to_binary({?ERL_LUAL_DOFILE, Filename})),
	receive_simple_response().

call(L, Args, Results) ->
    command(L, {?ERL_LUA_CALL, Args, Results}),
    receive_simple_response().
    
concat(L, N) ->
    command(L, {?ERL_LUA_CONCAT, N}).

dump_table(L, N) ->
	dump_table(L, N, none).

dump_table(L, N, NextKey) ->
	io:format("dump_table(~p, ~p)~n", [N, NextKey]),
	case NextKey of
		none ->
			Str=lists:concat(["tmpkey, tmpval = pairs(", N, ")(", N, ")"]);
		NK ->
			Str=lists:concat(["tmpkey, tmpval = pairs(", N, ")(", N, ", \"", NK, "\")"])
	end,
	io:format("Str == ~p~n", [Str]),
	lual:dostring(L, Str),
	lua:getglobal(L, "tmpkey"),
	{ok, T} = lua:type(L, -1),
	case T of
		?LUA_TNIL -> [];
		_ ->
			{ok, K} = lua:tolstring(L, -1),
			lua:remove(L, -1),
			lua:getglobal(L, "tmpval"),
			{ok, VT} = lua:type(L, -1),
			io:format("type == ~p~n", [VT]),
			case VT of
				?LUA_TNUMBER ->
					{ok, V} = lua:tonumber(L, -1);
				?LUA_TTABLE ->
					V = dump_table(L, lists:concat([N, ".", K]));
				_ ->
					{ok, V} = lua:tolstring(L, -1)
			end,
			lua:remove(L, -1),
			[{list_to_atom(K),V}|dump_table(L, N, K)]
	end.

getfield(L, global, Name) ->
    getglobal(L, Name);
getfield(L, Index, Name) ->
    command(L, {?ERL_LUA_GETFIELD, Index, Name}),
    receive_simple_response().
    
getglobal(L, Name) ->
    command(L, {?ERL_LUA_GETGLOBAL, Name}),
    receive_simple_response().

gettable(L, global, Name) when is_atom(Name) ->
	gettable(L, global, atom_to_list(Name));
gettable(L, global, Name) ->
	getfield(L, global, Name),
	{ok, T} = gettop(L),
	Table = gettable(L, T),
	lua:remove(L, T),
	Table.

gettable(L, T) ->
	pushnil(L),
	gettablekey(L, T).

gettablekey(L, T) ->
	next(L, T),
	{ok, OT} = gettop(L),
	case OT of
		T ->
			[];
		_ ->
      %% values can be tables or anything else
      %% recurse on tables
			case type_atom(L, -1) of
				{ok, table} ->
          %% keys can be strings or numbers
          case type_atom(L, -2) of 
            {ok, number} -> 
              {ok, Key} = tonumber(L, -2);
            _KT -> 
              {ok, Key} = tolstring(L, -2)
          end,
					KV = {Key, gettable(L, T + 2)},
					remove(L, -1),
					[KV|gettablekey(L, T)];
				_TA ->
					{ok, _Type, Val} = pop(L),
          %% keys can be strings or numbers
          case type_atom(L, -1) of 
            {ok, number} -> 
              {ok, Key} = tonumber(L, -1);
            _KT -> 
              {ok, Key} = tolstring(L, -1)
          end,
					[{Key, Val}|gettablekey(L, T)]
		end
	end.

gettop(L) ->
    command(L, {?ERL_LUA_GETTOP}),
    receive_valued_response().
    
next(L, Index) ->
	command(L, {?ERL_LUA_NEXT, Index}),
	receive_simple_response().

pop(L) ->
		{ok, R} = gettop(L),
		if
			R < 1 ->
				{ok, empty};
			true ->
				{ok, T} = type_atom(L, R),
				case T of
					number ->
						{ok, N} = tonumber(L, R),
						remove(L, R),
						{ok, number, N};
					string ->
						{ok, N} = tolstring(L, R),
						remove(L, R),
						{ok, string, N};
					boolean ->
						{other, N} = toboolean(L, R),
						remove(L, R),
						{ok, boolean, N};
					function ->
						remove(L, R),
					  {ok, function, function};
					_ ->
						remove(L, R),
						{ok, unknown, unknown}
				end
		end.

push(L, Term) when is_number(Term) ->
	pushnumber(L, Term);
push(L, Term) when is_list(Term) ->
	pushstring(L, Term);
push(L, true) ->
	pushboolean(L, true);
push(L, false) ->
	pushboolean(L, false);
push(L, Term) when is_atom(Term) ->
	pushstring(L, atom_to_list(Term)).

pushboolean(L, Bool) ->
    command(L, {?ERL_LUA_PUSHBOOLEAN, Bool}),
    receive_simple_response().
    
pushinteger(L, Int) when is_integer(Int) ->
    command(L, {?ERL_LUA_PUSHINTEGER, Int}),
    receive_simple_response().

pushstring(L, String) when is_list(String) ->
    command(L, {?ERL_LUA_PUSHSTRING, String}),
    receive_simple_response().

pushnil(L) ->
    command(L, {?ERL_LUA_PUSHNIL}),
    receive_simple_response().
    
pushnumber(L, Num) when is_number(Num) ->
    command(L, {?ERL_LUA_PUSHNUMBER, Num}),
    receive_simple_response().

remove(L, Index) ->
    command(L, {?ERL_LUA_REMOVE, Index}),
    receive_simple_response().
    
setfield(L, global, Name) ->
    setglobal(L, Name);
setfield(L, Index, Name) ->
    command(L, {?ERL_LUA_SETFIELD, Index, Name}),
    receive_simple_response().

setglobal(L, Name) ->
    command(L, {?ERL_LUA_SETGLOBAL, Name}),
    receive_simple_response().

toboolean(L, Index) ->
    command(L, {?ERL_LUA_TOBOOLEAN, Index}),
    receive_valued_response().

tointeger(L, Index) ->
    command(L, {?ERL_LUA_TOINTEGER, Index}),
    receive_valued_response().

tolstring(L, Index) ->
    command(L, {?ERL_LUA_TOLSTRING, Index}),
    receive_valued_response().

tonumber(L, Index) ->
    command(L, {?ERL_LUA_TONUMBER, Index}),
    {ok, Value} = receive_valued_response(),
    Value2 = list_to_binary(Value),
    {ok, binary_to_term(Value2)}.

type(L, Index) ->
    command(L, {?ERL_LUA_TYPE, Index}),
    receive_valued_response().

type_atom(L, Index) ->
	command(L, {?ERL_LUA_TYPE, Index}),
	R = receive_valued_response(),
	case R of
		{ok, Str} ->
			{ok, type_int_to_atom(Str)};
		_ ->
			R
	end.

type_int_to_atom(TypeInt) when is_integer(TypeInt) ->
	case TypeInt of
		0 ->
			Atom = nil;
		1 ->
			Atom = boolean;
		2 ->
			Atom = light_user_data;
		3 ->
			Atom = number;
		4 ->
			Atom = string;
		5 ->
			Atom = table;
		6 ->
			Atom = function;
		7 ->
			Atom = user_data;
		8 ->
			Atom = thread;
		_ ->
			Atom = unknown
	end,
	Atom.
	
command(#lua{port=Port}, Data) ->
    port_command(Port, term_to_binary(Data)).

receive_simple_response() ->
    receive
        ok ->
            ok;
        error ->
            {error, lua_error};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.
    
receive_valued_response() ->
    receive
        {ok, Str} ->
            {ok, Str};
        error ->
            {error, lua_error};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.
