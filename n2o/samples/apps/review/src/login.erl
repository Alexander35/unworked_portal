-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="login",app=review,bindings=[{body,body()}]}.
body() ->
	[ 
            #panel{class = <<"cols col-7"/utf8>>, body = "Login: "},
	    #panel{class = <<"cols col-2"/utf8>>, body = [<<"Поле   "/utf8>> ,  #textbox{id=user,autofocus=false}]}	
     	].
