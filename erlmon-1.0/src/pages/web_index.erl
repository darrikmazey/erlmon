-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon home.".

body() ->
	[
		#h1{text="Node Status"},
		get_node_status()
	].


menu_items() -> helper:menu([{home,"dashboard","/"},{config,"configuration","/web/config"}]).
	
event(_) -> ok.

get_node_status() ->
	Nodes = node:status(),
	node_status_to_html(Nodes).

node_status_to_html([{Node, Status}|T]) ->
	[
		#p{},
		#label{text=lists:flatten(io_lib:format("~s : ~s", [atom_to_list(Node), case Status of
			up -> "UP";
			down -> "DOWN";
			_ -> "UNKNOWN" end]))}
	| node_status_to_html(T)];
node_status_to_html([]) ->
	[].
