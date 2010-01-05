
-module(debug_console).
-author(darrik@darmasoft.com).

-export([log/3]).

log(Format, Args, _Options) ->
	case is_list(Args) of
		false -> io:format(Format ++ "~n", [Args]);
		true -> io:format(Format ++ "~n", Args)
	end.

