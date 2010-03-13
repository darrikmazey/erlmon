-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"web_index".

body() ->
	#label{text="web_index body."}.

menu_items() -> helper:menu([{home,"Home","/"},{nodes,"Nodes","/nodes"}]).
	
event(_) -> ok.
