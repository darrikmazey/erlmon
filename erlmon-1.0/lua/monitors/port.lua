-- Port Monitoring
-- if start/stop/restart are strings, they will be executed as system commands
-- if they are functions they will be called as necessary.
-- by default, restart simply calls stop, then start
function _add_monitor_port_function(host_monitors)
  host_monitors.monitor_port = function(hostorport,port,options)
    if port == nil then
      port = hostorport
      hostorport = 'localhost'
    end
    if options == nil then
      options = {}
    end

    options.host = hostorport
    options.port = port

    return host_monitors.add("port",nil,options)
  end
end

