-- this require isn't specifically required as it will be added
-- when executed inside of erlmon, but they are necessary to 
-- test the file on the command line, by typing lua config.lua
require "lua/erlmon"

 
-- Quick lua tutorial:
-- A table can be created like this: mytable = { a = 1, b = 2 }
-- then i can say mytable.a, or mytable['a']... you'll see this syntax
-- all over the file. Also, tables can have 'metatables'. erlmon 
-- uses this extensively to lookup global properties if they're not
-- overridden on a sub object. details below...
  
-- This is where globals go. 

Erlmon.http.login = 'admin'
Erlmon.http.password = 'admin'
Erlmon.alert.email = 'me@myemail.com'

-- Specific config differences can go here

-- sigmund - memcached
sigmund = Erlmon.add_host("sigmund.mydomain.com")

-- In this example, sigmund is a lua table. if you access a property 
-- of sigmund that doesn't exist, the equivalent property will be accessed 
-- on the global Erlmon object.
 
sigmund.http.password = "superego"
sigmund.alert.email = "sigmundalerts@myemail.com"

sigmund.monitor_process("memcached", 
  { start = "/etc/init.d/memcached start", 
    stop = "/etc/init.d/memcached stop"
  }
)

sigmund.monitor_port(11211) -- memcached

