-module (web_config).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"erlmon configuration".

alert_status() -> 
  Address = config:setting([smtp,address]),
  case length(Address) of
    0 -> "no alert address set!";
    _  -> ["alerts sent to " ++ Address]
  end.

body() ->
  case file:read_file("config.lua") of
		{ok, Config} ->
			[
				#h1{text="Configuration"},
				#p{},
				#label{text="Edit the configuration file for this node. Updates are applied immediately to all nodes."},
				#textarea {id=config, text=binary_to_list(Config), html_encode=false },
        #p{},
        #button { text="Save Config", postback={click, save_button} }

			];
		{error, _Reason} ->
			[
				#h1{text="Configuration"},
				#p{},
				#label{text="config.lua is either missing or inaccessible.  Check the file and try again, please."}
			]
	end.

menu_items() -> helper:menu([{home,"dashboard","/"},{nodes,"configuration","/web/config"}]).
	
event(_) -> 
  [Config] = wf:q(config),
  config:update_file(Config),
  helper:flash_msg("Config reloaded."),
  wf:redirect("/web/config"),
  ok.

