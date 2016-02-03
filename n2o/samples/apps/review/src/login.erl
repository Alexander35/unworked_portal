-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="login",app=review,bindings=[{body,body()}]}.
body() ->
	[
	    #link{class=["carousel-control", left], url="#"++"ID", postback=show, body="BODYLINK"} 
            %%#panel{class = <<"cols col-7"/utf8>>,  postback=show1,body = "Login: "}
	    %%#panel{class = <<"cols col-2"/utf8>>, body = [<<"Поле   "/utf8>> ,  #textbox{id=user,autofocus=false}]}	
     	].
event(show) ->
    io:format("~p",["dfg"]);
event(show1) ->
    io:format("~p",["111"]).
