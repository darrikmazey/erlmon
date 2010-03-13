-- this require isn't specifically required as it will be added
-- when executed inside of erlmon, but they are necessary to 
-- test the file on the command line, by typing lua config.lua
require "lua/erlmon"

http.login = 'foo'
http.password = 'bar'

monitor_process("memcached", 
  { start = "memcached", 
    stop = "killall memcached"
  })

