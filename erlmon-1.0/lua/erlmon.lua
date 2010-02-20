

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

function _erlmon()

  -- HTTP Console defaults
  http = {}
  http.enabled=true
  http.port = 9494

  -- Monitors
  monitors = {}
  monitors.list = {}
  monitors.add = _add
  monitors.remove = _remove

  -- Globals
  erlmon = {}
  erlmon.http = http
  erlmon.monitors = monitors

  return erlmon

end

Erlmon = _erlmon()


