
%% add node msg
-record(msg_nodesrv_add_node, {sender, node}).

%% remove node msg
-record(msg_nodesrv_rem_node, {sender, node}).
