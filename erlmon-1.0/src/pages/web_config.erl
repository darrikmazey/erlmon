-module (web_config).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon configuration".

body() ->
  {ok, Config} = file:read_file("/home/darrik/wdir/erlmon/erlmon-1.0/config.lua"),
	debug:log("Config == ~p", [Config]),
  [
    #h1{text="Configuration"},
    #p{},
    #label{text="Edit the configuration file for this node."},
    #textarea { text=binary_to_list(Config), html_encode=false }

  ].

menu_items() -> helper:menu([{home,"dashboard","/"},{nodes,"configuration","/web/config"}]).
	
event(_) -> ok.
