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
	#table { rows=[
		#tablerow { cells = [
			#tableheader { text="Node" },
			#tableheader { text="Status" }
		]},
	node_status_to_html(Nodes)
	]}.

node_status_to_html([{Node, Status}|T]) ->
	{Text, Class} = case Status of
		up ->
			{"UP", "node_status_up"};
		down ->
			{"DOWN", "node_status_down"};
		_ ->
			{"UNKNOWN", "node_status_unknown"}
	end,
	[
		#tablerow { cells = [
			#tablecell { text=atom_to_list(Node) },
			#tablecell { text=Text, class=Class }
		]}
	| node_status_to_html(T)];
node_status_to_html([]) ->
	[].
