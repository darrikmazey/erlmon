-module (web_config).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon configuration".

body() ->
  case file:read_file("/home/darrik/wdir/erlmon/erlmon-1.0/config.lua") of
		{ok, Config} ->
			[
				#h1{text="Configuration"},
				#p{},
				#label{text="Edit the configuration file for this node."},
				#textarea { text=binary_to_list(Config), html_encode=false }

			];
		{error, Reason} ->
			[
				#h1{text="Configuration"},
				#p{},
				#label{text="config.lua is either missing or inaccessible.  Check the file and try again, please."}
			]
	end.

menu_items() -> helper:menu([{home,"dashboard","/"},{nodes,"configuration","/web/config"}]).
	
event(_) -> ok.
