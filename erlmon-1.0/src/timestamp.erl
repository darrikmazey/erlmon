
-module(timestamp).
-author(darrik@darmasoft.com).

-export([now/0]).

now() ->
	{Date, Time} = calendar:local_time(),
	{Y, M, D} = Date,
	{Hours, Mins, Secs} = Time,
	lists:flatten(io_lib:format("~4.4.0s~s~2.1.0s~s~2.1.0s~s~2.1.0s~s~2.1.0s~s~2.1.0s",
		[ integer_to_list(Y),
			".",
			integer_to_list(M),
			".",
			integer_to_list(D),
			" ",
			integer_to_list(Hours),
			":",
			integer_to_list(Mins),
			":",
			integer_to_list(Secs) ])).
