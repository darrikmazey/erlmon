-module (web_config).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon configuration".

body() ->
  [
    #h1{text="Configuration"},
    #p,
    #label{text="Edit the configuration file for this node."},
    #textarea { text=Config }

  ].

menu_items() -> helper:menu([{home,"dashboard","/"},{nodes,"configuration","/web/config"}]).
	
event(_) -> ok.
