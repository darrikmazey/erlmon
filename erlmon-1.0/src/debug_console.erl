
-module(debug_console).
-author(darrik@darmasoft.com).

-export([log/3]).

log(Format, Args, _Options) ->
	io:format(Format ++ "~n", Args).

