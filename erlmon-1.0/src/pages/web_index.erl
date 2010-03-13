-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon home.".

body() ->
	#label{text="web_index body."}.

menu_items() -> helper:menu([{home,"dashboard","/"},{nodes,"configuration","/config"}]).
	
event(_) -> ok.
