
-- Monitors
-- To write a monitor, you must implement the monitor_X method,
-- where X is the name of the monitor.

-- $TODO should this be a macro? 
start = "start"
stop = "stop"
restart = "restart"

-- Process Monitoring
-- if start/stop/restart are strings, they will be executed as system commands
-- if they are functions they will be called as necessary.
-- by default, restart simply calls stop, then start
function monitor_process(name,startcmd,stopcmd,restartcmd) 

  -- we need a table with the commands, so build from parameters
  -- if needed
  if not (type(startcmd) == "table") then
    startcmd = { start = startcmd,
                 stop = stopcmd,
                 restart = restartcmd }
  end

  Erlmon.monitors.add("process",name,startcmd)

end

