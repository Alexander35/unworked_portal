-module(login1).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
	#dtl{file="login1",app=review,bindings=[{body,body()}]}.

body() ->
	[ 
		#table{style="width:50%",class=[table,"table-hover"], 
			body=[[
				#tr{cells=[
					#th{colspan = 2, body= #panel{id=conn_status, body=["Enter your Username and Password"]} }
				]},
				#tr{cells=[
                                        #td{body= #span{body="Username: "} },
                                        #td{body= #textbox{style="width:100%", id=user,autofocus=true} }
                                ]},
				#tr{cells=[
                                        #td{body= #span{body="Password: "} },
                                        #td{body= #input{style="width:100%", type=password,id=pass} }
                                ]},
				#tr{cells=[
					#td{body= " "},
                                        #td{body= #button{body=">>>",postback=login,source=[user,pass],autofocus=true} }
                                ]}
			]]
		} 
	].
init_user(User,Pass)->
    case db_suite:key_test(User, Pass) of
        {ok, Name, _Comment} ->
                               wf:user(Name),
                               wf:redirect("find_port_via_mac");
        _ ->
                               wf:update(conn_status, #panel{id=conn_status, body=["Error: user/pass not found! please try again"]})
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
