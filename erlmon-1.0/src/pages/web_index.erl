-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"web_index".

body() ->
	#label{text="web_index body."}.

menu_items() -> 
          "<ul>
            <li class='active'><a href='/'>Home</a></li>
          </ul>".
	
event(_) -> ok.
