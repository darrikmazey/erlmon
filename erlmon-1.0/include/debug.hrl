
% log message
-record(debug_log_msg, { sender, format, args = [] }).

% add logmethod
-record(debug_log_add_method, { sender, module, options }).

% remove logmethod
-record(debug_log_rem_method, { sender, module, options }).

