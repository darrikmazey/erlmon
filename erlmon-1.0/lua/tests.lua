require "lunit"
require "erlmon"

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

function test_port_monitor()
  monitor_port(22)
  monitor_port("localhost",22)
  print(Erlmon.monitors.list)
  assert(#Erlmon.monitors.list == 2,#Erlmon.monitors.list)
end


