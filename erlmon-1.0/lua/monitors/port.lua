-- Port Monitoring
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

    -- 'tcp_port' becuase must match erlang monitor name
    return host_monitors.add("tcp_port",nil,options)
  end
end

