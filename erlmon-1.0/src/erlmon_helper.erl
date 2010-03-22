
-module(erlmon_helper).
-author(darrik@darmasoft.com).

-export([
	list_to_number/1
]).

list_to_number(L) ->
	try list_to_float(L) of
		N -> N
	catch
		_:_ -> list_to_integer(L)
	end.
