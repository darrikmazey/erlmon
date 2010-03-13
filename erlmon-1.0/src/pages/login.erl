-module (login).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon login.".

body() ->
  [#h1{text = "Login to erlmon." },
    #label { text="Login:"},
    #textbox { id=login, text="", next=password },
    #label { text="Password:"},
    #password { id=password},
    #p{},
    #button { text="Login", postback={click, login_button} }
  ].

event(_) -> 
  [Login] = wf:q(login),
  [Password] = wf:q(password),
  case config:authenticate(Login,Password) of
    true -> wf:user(admin),
          wf:redirect_from_login("/");
    false -> helper:flash_error("Invalid Login")
  end.


menu_items() -> helper:menu([{login,"login","/web/login"}]).

