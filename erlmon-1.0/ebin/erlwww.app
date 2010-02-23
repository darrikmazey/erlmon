{application, erlwww, [
	{description,  "Nitrogen Website"},
	{mod, {erlwww_app, []}},
	{env, [
		{platform, mochiweb}, %% {inets|yaws|mochiweb}
		%%{port, 8000},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]}
]}.
