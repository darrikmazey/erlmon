-module (helper).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

menu(MenuItems) -> 
  Items = lists:map(fun({_Name,Text,Url}) -> 
    ["<li>",wf:f("<a href=~p>~s</a>",[Url,Text]),"</li"] end,MenuItems),
  ["<ul>",Items,"</ul>"].


