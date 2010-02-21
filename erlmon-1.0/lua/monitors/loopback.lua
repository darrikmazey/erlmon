
-- "loopback" Monitor
-- monitor for unit tests
-- if they are functions they will be called as necessary.
-- by default, restart simply calls stop, then start
function monitor_loopback(name,cmd)

  Erlmon.monitors.add("loopback",name,cmd)

end

function unmonitor_loopback(name)
  Erlmon.monitors.remove("loopback",name)
end

