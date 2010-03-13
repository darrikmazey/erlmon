
function authenticate(login,password)
  return Erlmon.http.login == login and Erlmon.http.password == password
end
