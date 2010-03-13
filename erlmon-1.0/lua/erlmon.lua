
-- helper functions for monitors

function _add(mtype,name,init)
  mlist = Erlmon.monitors.list

  -- create table if this is the first monitor of this type
  if mlist[mtype] == nil then mlist[mtype] = {} end

  -- TODO - how do we handle the preexisting monitor? for now we just override it
  -- save new monitor
  mlist[mtype][name] = init
  
end

function _remove(mtype,name)
  if mlist[mtype] == nil then mlist[mtype] = {} end
  mlist[mtype][name] = nil
end

function _add_host(name)
  local host = {}
  local mt = {}
  mt.__index = function (table, key)
    return Erlmon[key]
  end
  mt.__newindex = function (table, key)
    return Erlmon[key]
  end
  setmetatable(host,mt)
  Erlmon.hosts[name] = host
  return host
end

function _erlmon()

  -- HTTP Console defaults
  local http = {}
  http.enabled=true
  http.port = 9494

  -- Monitors
  local monitors = {}
  monitors.list = {}
  monitors.add = _add
  monitors.remove = _remove

  -- Simple SMTP Alerting
  local alert = {}
  alert.email = ""

  -- Globals
  local erlmon = {}
  erlmon.http = http
  erlmon.monitors = monitors
  erlmon.alert = alert
  erlmon.add_host = _add_host
  erlmon.hosts = {}

  return erlmon

end

Erlmon = _erlmon()

-- load monitors, which may need the global Erlmon table
require "lua/monitors/monitors"

-- authentication
require "lua/authentication"
