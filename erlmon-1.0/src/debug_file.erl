
-module(debug_file).
-author(darrik@darmasoft.com).

-export([log/3]).

log(Format, Args, Options) ->
	NewArgs = case is_list(Args) of
		true -> Args;
		false -> [Args]
	end,
	{filename, Filename} = Options,
	{ok, WD} = file:open(Filename, [append]),
	io:format(WD, Format ++ "~n", NewArgs),
	file:close(WD),
	true.


