-- this require isn't specifically required as it will be added
-- when executed inside of erlmon, but they are necessary to 
-- test the file on the command line, by typing lua config.lua
require "lua/erlmon"

-- this is where globals go
Erlmon.http.login = 'admin'
Erlmon.http.password = 'admin'
