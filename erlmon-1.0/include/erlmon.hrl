
%% announce
-record(node_announce, {sender, pid, node, state}).


%% state change
-record(state_change, {sender, node, objtype, obj, prev_state, new_state, callback=none, data=none, ts}).

%% announce
-record(storage_announce, {sender, node}).
%% ack announce
-record(storage_ack_announce, {sender, node}).

%% test_row
-record(test, {key, value}).

%% process
-record(process, {user, pid, cpu, mem, vsz, rss, tty, stat, start, time, cmd}).

%% filesystem
-record(filesystem, {path, size, used, avail, percent, mount}).
