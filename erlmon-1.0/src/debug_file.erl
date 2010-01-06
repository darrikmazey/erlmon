
-module(debug_file).
-author(darrik@darmasoft.com).

-export([log/3]).

log(Format, Args, Options) ->
	{filename, Filename} = Options,
	{ok, WD} = file:open(Filename, [append]),
	io:format(WD, Format ++ "~n", Args),
	file:close(WD),
	true.


