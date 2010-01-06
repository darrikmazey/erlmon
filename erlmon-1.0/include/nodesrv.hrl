
%% list nodes msg
-record(msg_nodesrv_list_nodes, {sender}).

%% node list msg
-record(msg_nodesrv_node_list, {sender, nodes}).

%% add node msg
-record(msg_nodesrv_add_node, {sender, node}).

%% remove node msg
-record(msg_nodesrv_rem_node, {sender, node}).

%% announce msg
-record(msg_nodesrv_announce, {sender, node, nodesrv}).
