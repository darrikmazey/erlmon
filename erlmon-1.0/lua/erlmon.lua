
-- helper functions for monitors

-- load monitors
require "lua/monitors/monitors"

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
  _add_monitors(monitors)

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

_add_convenience_monitor_functions()

-- redefine the method in your config file if you want 
-- to have your own authentication 
require "lua/authentication"
