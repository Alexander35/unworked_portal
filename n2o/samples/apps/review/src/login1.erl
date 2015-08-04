-module(login1).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="login1",app=review,bindings=[{title,<<"логин | ИОН"/utf8>>},{body,body()}]}.

body() ->
	[ 
		
		#panel{ class= <<"cols col-12full"/utf8>>, body=#panel{id=conn_status, body= <<"Введите логин и пароль"/utf8>> }},
		#panel{ class= <<"cols col-6resize"/utf8>>, body= [<<"Пользователь: <br>"/utf8>>, #textbox{style="width:100%", id=user,autofocus=true}]},
		#panel{ class= <<"cols col-6resize"/utf8>>, body= [<<"Пароль : <br>"/utf8>>, #input{style="width:100%", type=password,id=pass}]},
		#panel{ class= <<"cols col-12full"/utf8>>, body=#button{body=">>>",postback=login,source=[user,pass],autofocus=true}}

	].
init_user(User,Pass)->
    case db_suite:key_test(User, Pass) of
        {ok, Name, _Comment} ->
                               wf:user(Name),
                               wf:redirect("find_port_via_mac");
        _ ->
                               wf:update(conn_status, #panel{id=conn_status, body= <<"Логин и/или пароль не совпадают. Пожалуйста, попробуйте ещё раз"/utf8>> })
    end.

event(login) ->
	%%case db_suite:key_test(wf:q(user), wf:q(pass)) of
	%%{ok, Name, _Comment} -> 
	%%		       wf:user(Name),
	%%		       wf:redirect("find_port_via_mac");
	%%_ ->
	%%		       wf:update(conn_status, #panel{id=conn_status, body=["Error: user/pass not found! please try again"]})
    %%end;
	init_user(wf:q(user),wf:q(pass));

event(_) -> [].
