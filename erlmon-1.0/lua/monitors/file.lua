-- File Monitor
-- To write a monitor, you must implement the monitor_X method,
-- where X is the name of the monitor.

function monitor_file(name,startcmd,stopcmd,restartcmd) 

  -- we need a table with the commands, so build from parameters
  -- if needed
  if not (type(startcmd) == "table") then
    startcmd = { start = startcmd,
                 stop = stopcmd,
                 restart = restartcmd }
  end

  Erlmon.monitors.add("file",name,startcmd)

end


