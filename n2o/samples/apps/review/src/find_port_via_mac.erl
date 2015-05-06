-module(find_port_via_mac).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
	case wf:user() /= undefined of %%user recognize  
    		true  -> 			 
			 #dtl{
				file = "find_port_via_mac",
			      	app=review,
			      	bindings=[{title,<<"FIND_PORT_VIA_MAC">>},{body,body()}]
			     }; %%rendering page
    		false -> wf:redirect("login1")
	end.

body() ->
	[
		#table{style="width:100%",class=[table,"table-hover"],
			body=[[
				#tr{cells=[
	 				#td{colspan=4,body =#span{body= "User : "++wf:user()}}
				]},
				#tr{cells=[
					#td{body=#panel{id=label1, body = <<"Traget list : ">>}}, 
					#td{body=#panel{id=search_list, body = << "[List is empty]" >>  }},
					#td{body=#button{
                                                id=connect,
                                                body="connect",
                                                postback=connect
                                        }},
					#td{body=#button{
                                                id=logout,
                                                body="logout",
                                                postback=logout
                                        }}
				]},
				#tr{cells=[
					#td{body=#panel{id=label2, body = <<"MAC address: ">>}},
					#td{body=#textbox{id=mac, value = <<"00-00-DB-00-00-DB">>, autofocus=true}},
					#td{body=#button{
                                                id=find,
                                                body="find MAC",
                                                postback=finding,
                                                source=[mac,ip,mark,mark_id]
                                        }},
					#td{body=#button{
                                                id=find_up_port,
                                                body="find UP port",
                                                postback=finding_up_port,
                                                source=[ip,mark,mark_id]
                                        }}
      	 	      		]},
				#tr{cells=[ 
					#td{body=#panel{id=label3, body = <<"Target IP: ">>}}, 
					#td{body=#textbox{id=ip_list, value = <<"192.168.20.44">>}},
					#td{body=#button{
                                                id=add_ip,
                                                body="add IP",
                                                postback=add_ip,
                                                source=[ip_list]
                                        }},
                                        #td{body=#button{
                                                id=del_ip,
                                                body="del IP",
                                                postback=del_ip,
                                                source=[ip_list]
                                        }}					
				]},
				#tr{cells=[
					#td{body=#panel{id=label3, body= <<"Marker">>}},
					#td{body=#textbox{id=mark_id, value = <<"Current_ID">>}},
					#td{body=#textbox{id=mark, value = <<"1-1-1(1-2)">>}},
					#td{body=#button{
                        			id=next_mark,
                        			body="o-0-0(O-o)",
                        			postback=get_next_mark,
                        			source=[mark]
                			}}
				]},
				#tr{cells=[
					#td{colspan=4,body=#panel{id=status_bar,  body=["status:"]}}
				]},
				#tr{cells=[
					#td{colspan=4,body=#panel{id=telnet_out,  body=["telnet_out_here"]}}
				]},
				#tr{cells=[
					#td{colspan=4,body=#panel{id=new_content, body=["body"]}}
				]},
				#tr{cells=[
					#td{colspan=4,body=#panel{id=db_out,  body=["cross-connect table"]}}
				]}
			]]
		}	
    	].
listener(ID)->
	receive
		{status_out, Cmd} ->
                        wf:update(status_bar, #panel{id=status_bar, body = Cmd }),
                        wf:flush(ID),
                        listener(ID);
		{cmd_out, Cmd} ->
			wf:update(telnet_out, #panel{id=telnet_out, body= wf:f("~s",[Cmd]) }),
			wf:flush(ID),
			listener(ID);
	
		{conn_error, Target} ->
			wf:update(status_bar, #panel{id=status_bar, body= "Connection error with "++Target }),
                        wf:flush(ID);
		
		{stop} ->
			wf:update(status_bar, #panel{id=status_bar, body= "stop signal recieved" }),
                        wf:flush(ID);

		{connection_lost}->
			wf:update(status_bar, #panel{id=status_bar, body= "Connection lost. Try to reconnect" }),
                        wf:flush(ID);
		
		{update_cross_table}->
			%%read_cross(),
			%%wf:flush(ID),
                        listener(ID);
		
		_Other ->
			some_other
	end.
read_cross()->
	read_cross(wf:session(conn_list),[],[]).
read_cross([],L,La)->
	wf:update(new_content, #panel{id=new_content, body=[#p{body= La }]}),
	wf:update(db_out, #panel{id=db_out, body=[#p{body= L }]}),
        wf:flush(list_to_atom(wf:user()));

read_cross([TargetTuple|Rest],L,La)->
	{Target,_,_,_}=TargetTuple,
	[{listing,port_edge_core28,PortList,standard_switch_port}]=db_suite:get_from(listing, port_edge_core28),
	{L1,La1}=read_cross(Target,PortList,["</table>"],[["</table>"]]),
	read_cross(Rest,[L1|L],[La1|La]).

read_cross(Target,[],List,Last)->
	{["<table border=1 style=width:100%><tr><td>Port ID</td><td>Port Desc</td><td>Mark</td><td>MAC</td><td>Test time</td><td>Mark time</td><td>User</td><td>Comment</td></tr>"|List],
	 ["<table border=1 style=width:100%><tr><td colspan=8>Last in "++Target++" </tr><tr><td>Port ID</td><td>Port Desc</td><td>Mark</td><td>MAC</td><td>Test time</td><td>Mark time</td><td>User</td><td>Comment</td></tr>"|Last]};

read_cross(Target,[Port|Ports],List,Last)->
	case db_suite:get_from(cross_actions,Target++":"++Port) of
		[{cross_actions,ID,
                [{test,{Yt,Mt,Dt},{Ht,Mit,St}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],
                UserID}] ->
			[{port_desc_string,_,Desc_Str,_}]=db_suite:get_from(port_desc_string,ID),
			[{cross_mark,_,Mark,Comment}]=db_suite:get_from(cross_mark,ID),
			case db_suite:get_from(cross_mac,ID) of
				[] -> 
					Mac="no mac";
				[{cross_mac,_,Mac,_}] ->
					ok;
				_Els -> 
					Mac="unk mac"
					
			end,
						String="<tr><td>"++ID++"</td><td>"++lists:concat(Desc_Str)++"</td><td>"++Mark++"</td><td>"++Mac++"</td><td>"
                                                        ++integer_to_list(Dt)++"."++integer_to_list(Mt)++"."++integer_to_list(Yt)++"  "
							++integer_to_list(Ht)++":"++integer_to_list(Mit)++":"++integer_to_list(St)++"</td><td>"
							++integer_to_list(Dm)++"."++integer_to_list(Mm)++"."++integer_to_list(Ym)++"  "
                                                        ++integer_to_list(Hm)++":"++integer_to_list(Mim)++":"++integer_to_list(Sm)++"</td><td>"++
							UserID++"</td><td>"++
						case is_atom(Comment) of
							true ->	atom_to_list(Comment);
							false -> Comment
						end ++
							"</td></tr>",
						case {date(),time()} of
							{{Ym,Mm,Dm},{Hm,Mim,_}}->
								Last1=[String|Last];
							_Else ->
								Last1=Last
						end;
		[] 	-> 
						String="<tr><td>"++Target++":"++Port++"</td><td colspan=7>[NONE]</td></tr>",
						Last1=Last;
		_Else 	-> 
						String="<tr><td>"++Target++":"++Port++"</td><td colspan=7>[Read Error!]</td></tr>",
						Last1=Last
	end,
	read_cross(Target,Ports,[String|List],Last1).

%% creates connection list
connection_list([]) ->
		wf:update(search_list, #panel{id=search_list, body= [["<br>"|TargetList] || {TargetList,_,_,_} <- wf:session(conn_list)] }),
		wf:flush(list_to_atom(wf:user()));

connection_list([H|Rest]) ->
	Pid = spawn(telnet_client,manager,[]),%%spawn listener and manager processes
	PidL = spawn(?MODULE,listener,[list_to_atom(wf:user())]),
	Pid ! {self(),PidL,conn_login,H}, %% send conn_login to manager 
	
	receive
		{From,Pidl,socket,Socket,Target}->
			wf:session(conn_list, [ {Target,From,Pidl,Socket} | wf:session_default(conn_list, []) ]  );

		{_,_,no_connect,_} ->
			ok		
	end,
	connection_list(Rest).

find_up_port([],_)->
        ok;

find_up_port([H|Rest],Mark)->
        {Ip,Pid,PidL,Socket} = H,
        Pid ! {PidL, Socket, sh_int_br, Ip, Mark, wf:user()},
        find_up_port(Rest,Mark).


find_port([],_,_)->
	ok;

find_port([H|Rest],Find,Mark)->
	{Ip,Pid,PidL,Socket} = H,
	Pid ! {PidL, Socket, sh_mac, "show mac-a | inc "++Find++"\n", Ip, Mark,wf:user()},
	find_port(Rest,Find,Mark).

%%event handlers
event(init) ->
	wf:reg(list_to_atom(wf:user()));

event(logout) ->
	case wf:session(conn_list) of
                undefined ->
                        ok;
                [] ->
                        ok;
                _Else ->
                        [ {From ! {stop, Sock}, Pidl ! {stop}} || {_,From,Pidl,Sock} <- wf:session(conn_list)],
                        wf:session(conn_list,[])
                        %%connection_list(wf:session(ipl))
        end;

event(get_next_mark) ->
	wf:update(mark, #textbox{id=mark, value = db_suite:next_mark([".",".",".",".","."],wf:q(mark))});

event(connect) ->
	case wf:session(conn_list) of
		undefined ->
			ok;
		[] ->
			ok;
                _Else -> 
			[ {From ! {stop, Sock}, Pidl ! {stop}} || {_,From,Pidl,Sock} <- wf:session(conn_list)],
			wf:session(conn_list,[])
			%%connection_list(wf:session(ipl))
	end,
	connection_list(wf:session(ipl)),
	find_port(wf:session(conn_list),"CA-01-15-90-00-08","Link?"),%%"The Truth Is Out There"
	read_cross();

event(finding_up_port) ->
        wf:update(mark_id, #textbox{id=mark_id, value = "Current_ID" }),
        case db_suite:get_from(cross_actions,wf:q(mark_id)) of
                [] ->
			find_up_port(wf:session(conn_list),wf:q(mark)),
                        timer:sleep(3000);
		
		[{cross_actions,ID,[_,{mark,{Y,M,D},{_,_,_}}],_}]->
                        case date() of
                                {Y,M,D} ->
                                        %%db_suite:add_port_str(ID,Str,no_comm),
                                        db_suite:add_action(ID,[{test,{Y,M,D},time()},{mark,{Y,M,D},time()}],wf:user()),
                                        db_suite:add_mark(ID, wf:q(mark), "Manually");
                                _Else -> ok
                        end;
                _Els -> ok
        end,
        read_cross();



event(finding) ->
	wf:update(mark_id, #textbox{id=mark_id, value = "Current_ID" }),
	case db_suite:get_from(cross_actions,wf:q(mark_id)) of
		[] ->
			find_port(wf:session(conn_list),wf:q(mac),wf:q(mark)),
                        timer:sleep(3000);

		[{cross_actions,ID,[_,{mark,{Y,M,D},{_,_,_}}],_}] -> 
			case date() of
				{Y,M,D} ->
					db_suite:add_action(ID,[{test,{Y,M,D},time()},{mark,{Y,M,D},time()}],wf:user()),
                                        db_suite:add_mark(ID, wf:q(mark), "Manually");
				_Else -> ok
			end;
		_Els -> ok
	end,
	read_cross();

%%don't forget optimizing this =)
event(add_ip) ->
	case wf:session(ipl) of
		undefined ->
			wf:session(ipl, [ wf:q(ip_list) | wf:session_default(ipl, []) ] ),
                      	wf:update(search_list, #panel{
                                      id = search_list,
                                      body = lists:map( fun(X) -> ["<br>"|X] end, wf:session( ipl ) )
                                   });
		_ -> 
			case lists:filter(fun(H) -> H == wf:q(ip_list) end, wf:session(ipl)) of
              			[] ->   wf:session(ipl, [ wf:q(ip_list) | wf:session_default(ipl, []) ] ),
                      			wf:update(search_list, #panel{
                                      					id=search_list,
                                      					body= lists:map( fun(X) -> ["<br>"|X] end, wf:session( ipl )) 
                                   				})
        		end
	end;

event(del_ip) ->
	wf:session(ipl, lists:delete( wf:q(ip_list) , wf:session(ipl))),
	wf:update(search_list, #panel{
					id=search_list,
					body= lists:map( fun(X) -> ["<br>"|X] end, wf:session( ipl ) )
				     });

event(_) ->
	wf:info(?MODULE, "unknown event called~n", []).
