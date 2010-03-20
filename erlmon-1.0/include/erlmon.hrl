
%% announce
-record(node_announce, {sender, pid, node, state}).


%% state change
-record(state_change, {sender, node, objtype, obj, prev_state, new_state, data=none, ts}).

%% sent alerts
-record(sent_alert, {node, to, objtype, obj, prev_state, new_state, ets, ats}).

%% state change filter
-record(state_change_filter, {state_change, fields=[]}).

%% alert filter
-record(alert_filter, {scf, to}).

%% announce
-record(storage_announce, {sender, node}).
%% ack announce
-record(storage_ack_announce, {sender, node}).

%% test_row
-record(test, {key, value}).

%% smtp_config
-record(smtp_config, {authtype, port, host, address, login, pass}).

%% --- monitor records --- %%

%% tcp port monitor
-record(tcp_port_monitor, {host, port, pid}).

%% process monitor
-record(process_monitor, {name, pid}).

%% filesystem
-record(filesystem, {path, size, used, avail, percent, mount}).

%% process
-record(process, {user, pid, cpu, mem, vsz, rss, tty, stat, start, time, cmd}).

