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
	{Text, Class, Cell} = case Status of
		up ->
			{"UP", "node_status_up", #tablecell { body=#link{ text=atom_to_list(Node), url=lists:flatten(io_lib:format("/web/node/~s", [atom_to_list(Node)]))}}};
		down ->
			{"DOWN", "node_status_down", #tablecell { text=atom_to_list(Node) }};
		_ ->
			{"UNKNOWN", "node_status_unknown", #tablecell { text=atom_to_list(Node) }}
	end,
	[
		#tablerow { cells = [
			Cell,
			#tablecell { text=Text, class=Class }
		]}
	| node_status_to_html(T)];
node_status_to_html([]) ->
	[].
