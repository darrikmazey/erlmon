-- if LuaFileSystem is OK as a dependency, we wouldn't have 
-- to specify the modules here
--require "lua/monitors/file"
--require "lua/monitors/process"
--require "lua/monitors/loopback"
require "lua/monitors/port"

function _add_monitors(host_monitors)

  local mlist = {} 
  host_monitors.list = mlist

  host_monitors.add = function(mtype,name,init)
    -- create table if this is the first monitor of this type
    if mlist[mtype] == nil then mlist[mtype] = {} end
    -- TODO - how do we handle the preexisting monitors? 
    mlist[mtype][name] = init
  end

  host_monitors.remove = function(mtype,name)
    if mlist[mtype] == nil then mlist[mtype] = {} end
    mlist[mtype][name] = nil
  end

  _add_monitor_port_function(host_monitors)
end

-- allows monitor_x instead of Erlmon.monitors
function _add_convenience_monitor_functions()
  monitor_port = Erlmon.monitors.monitor_port
end
