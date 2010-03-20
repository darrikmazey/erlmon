
function _add_smtp(host)
  local smtp = {}
  host.smtp = smtp
  smtp.auth = {}

  smtp.address = ""
  smtp.host = "localhost"
  smtp.port = 25
  smtp.auth.type = 'none'
  smtp.auth.username = ''
  smtp.auth.password = ''

end

-- convenience function if you just want to set a 
-- global alert address.
function alert(address)
  Erlmon.smtp.address = address
end
