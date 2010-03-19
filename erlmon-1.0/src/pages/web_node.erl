-module (web_node).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"node info".

body() ->
	Node = wf:get_path_info(),
	[
		#h1{text=lists:flatten(io_lib:format("Node Info : ~s", [Node]))},
		monitor_status_table(Node)
	].


menu_items() -> helper:menu([{home,"dashboard","/"},{config,"configuration","/web/config"}]).
	
event(_) -> ok.

monitor_status_table(Node) ->
	NodeAtom = list_to_atom(Node),
	HeaderRow = #tablerow { cells=[
		#tableheader { text="Object Type" },
		#tableheader { text="Object" },
		#tableheader { text="Status" }
	]},
	Rows = lists:flatten([
		HeaderRow,
		tcp_port_monitor_status_table(NodeAtom),
		process_monitor_status_table(NodeAtom)
	]),
	#table { rows=Rows }.

tcp_port_monitor_status_table(Node) ->
	case {node(), Node} of
		{Node, Node} ->
			Status = tcp_port_monitor:status();
		{_, Node} ->
			Status = rpc:call(Node, tcp_port_monitor, status, [])
	end,
	tcp_port_monitor_status_rows(Status).

tcp_port_monitor_status_rows([{ObjType,Host,Port,Pid}|T]) ->
	{ok, Status} = tcp_port_monitor:status(Pid),
	{Text, Class} = case Status of
		up -> {"UP", "monitor_status_up"};
		down -> {"DOWN", "monitor_status_down"};
		_ -> {Status, "monitor_status_unknown"}
	end,
	[	#tablerow { cells=[
			#tablecell { text=ObjType },
			#tablecell { text=lists:flatten(io_lib:format("~s:~p", [Host, Port])) },
			#tablecell { text=Text, class=Class }
		]} | tcp_port_monitor_status_rows(T) ];
tcp_port_monitor_status_rows([]) ->
	[].

process_monitor_status_table(Node) ->
	case {node(), Node} of
		{Node, Node} ->
			Status = process_monitor:status();
		{_, Node} ->
			Status = rpc:call(Node, process_monitor, status, [])
	end,
	process_monitor_status_rows(Status).

process_monitor_status_rows([{ObjType,ObjName,Pid}|T]) ->
	{ok, Status} = process_monitor:status(Pid),
	{Text, Class} = case Status of
		up -> {"UP", "monitor_status_up"};
		down -> {"DOWN", "monitor_status_down"};
		_ -> {Status, "monitor_status_unknown"}
	end,
	[
		#tablerow { cells=[
			#tablecell { text=ObjType },
			#tablecell { text=ObjName },
			#tablecell { text=Text, class=Class }
		]} | process_monitor_status_rows(T) ];
process_monitor_status_rows([]) ->
	[].
