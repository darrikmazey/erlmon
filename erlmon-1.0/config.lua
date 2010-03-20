-- this require isn't specifically required as it will be added
-- when executed inside of erlmon, but they are necessary to 
-- test the file on the command line, by typing lua config.lua
require "lua/erlmon"

-- Global Erlmon Settings
Erlmon.http.login = 'admin'
Erlmon.http.password = 'admin'

-- Global SMTP Settings
alert("chad@inakanetworks.com")

-- Monitors
monitor_port(11211)

