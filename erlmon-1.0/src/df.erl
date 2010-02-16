
-module(df).
-author(darrik@darmsoft.com).

-export([list/0]).
-export([list/1]).
-export([find_path/1]).

-include("include/erlmon.hrl").

list() ->
	list([]).

list([]) ->
	Output = os:cmd("df"),
	process(Output).

process(Output) when is_list(Output) ->
	Lines = re:split(Output, "\n", [{return, list}]),
	[_Header|T] = Lines,
	lists:flatten(create_records(T)).

create_records([H|[]]) ->
  [create_record(H)];
create_records([H|T]) ->
	[create_record(H) | create_records(T)];
create_records([]) ->
	[].

create_record([Head|[]]) -> create_record(Head);
create_record([]) -> [];
create_record(Line) ->
	Fields = re:split(Line, "\s+", [{return, list}]),
	[Path, Size, Used, Avail, Percent, Mount] = Fields,
	[NewPercent|_] = lists:flatten(re:replace(Percent, "%", "")),
	IntPercent = binary_to_integer(NewPercent),
	case re:run(Path, "/dev") of
		{match, _} ->
			Rec = #filesystem{path=Path, size=Size, used=Used, avail=Avail, percent=IntPercent, mount=Mount},
			Rec;
		nomatch -> []
	end.

binary_to_integer(Bin) ->
	case erl_scan:string(binary_to_list(Bin)) of
		{ok, Tokens, _} ->
			first_integer(Tokens);
		_ -> 0
	end.

first_integer([{integer, _V, I}|_T]) -> I;
first_integer([_H|T]) -> first_integer(T);
first_integer([]) -> 0.

find_path(Path) ->
	find_path(Path, list()).

find_path(Path, [H=#filesystem{path=Path}|_T]) ->
	H;
find_path(Path, [_H|T]) ->
	find_path(Path, T);
find_path(_Path, []) ->
	not_found.
