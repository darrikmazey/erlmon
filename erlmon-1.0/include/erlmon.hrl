
%% announce
-record(node_announce, {sender, pid, node, state}).


%% state change
-record(state_change, {sender, node, objtype, obj, prev_state, new_state, ts}).

%% announce
-record(storage_announce, {sender, node}).
%% ack announce
-record(storage_ack_announce, {sender, node}).

%% test_row
-record(test, {key, value}).
