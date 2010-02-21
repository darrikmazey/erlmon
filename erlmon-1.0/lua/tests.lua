require "lunit"
require "erlmon"
-- if LuaFileSystem is OK as a dependency, we wouldn't have to specify the modules here
require "monitors/monitors"


module( "erlmon_tests", lunit.testcase, package.seeall)

-- monitor infrastructure basics
function test_monitors()
  assert(not (Erlmon.monitors == nil))
  assert(type(Erlmon.monitors.list) == "table") 
  assert(type(Erlmon.monitors.add) == "function") 
  assert(type(Erlmon.monitors.remove) == "function") 
end

function test_http_config()
  assert(Erlmon.http.enabled)
  assert(Erlmon.http.port == 9494)
end

function test_loopback_monitor()
  monitor_loopback("foo","bar")
  assert(Erlmon.monitors.list.loopback)
  assert(Erlmon.monitors.list.loopback.foo == "bar")
  unmonitor_loopback("foo")
  assert(Erlmon.monitors.list.loopback.foo == nil)
end


