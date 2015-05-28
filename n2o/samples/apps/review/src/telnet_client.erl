-module(telnet_client).
-export([connect/1, disconnect/1, fetch/2, manager/0]).

%%interface manager
manager()->
	receive	
		%%connecting and logginig sequence starts here
		{From,PidL, conn_login, Target,UserID} -> 
				%%trying to connect
				case connect(Target) of
					{ok, Socket} ->
						Answer = fetch(Socket, []),
                                		Cmd = decode(Socket, Answer),					
						case Cmd /= [] of
							true ->
								case  lists:last(Cmd) of
                                                   			"Username: " ->
                                                              			ok = gen_tcp:send(Socket, "albiruni\nj642Trz\n"),
                                       		                		An = fetch(Socket,[]),
                                       		                		decode(Socket, An),
										ok = gen_tcp:send(Socket,
											"configure\nmac-address-table aging-time 10\nend\n"),
										fetch(Socket,[]),
										PidL ! {status_out,"connected"},
										From ! {self(),PidL,socket,Socket,Target};
										%%PidL ! {update_cross_table,UserID};
									_Any -> 
										From ! {self(),PidL,no_connect,Target},
										PidL ! {status_out,"connection: not connected"}
                                        			end,
								manager();
							false -> 
								From ! {self(),PidL,no_connect,Target},
								PidL ! {status_out,"connection : not connected"}
								%%everything else we thinks that connection is not established 
						end;

                			{error, _} -> 
							From ! {conn_error, Target},
							PidL ! {status_out,"connection : not connected - error"};
							%%if Target not reachable or not exist
					_Other -> 
							PidL ! {status_out,"connection : smtn else is called"}
				end;
						
			
		{stop, Sock} ->
			 disconnect(Sock);
		
		{From, Socket, sh_mac, Cmd, Ip, Mark, UserID} ->
			 From ! { status_out,"find mac : request to switch "++Ip },
			 ok = gen_tcp:send(Socket, Cmd),
                         An = fetch(Socket,[]),
			 [_|CC]=decode(Socket, An),%% cutting first element == command show mac-a
			 case lists:last(CC) of
				"--- [Space] Next page, [Enter] Next line, [A] All, Others to exit ---" ->
					%%From ! { status_out,"find mac : request to switch "++Ip },
					ok = gen_tcp:send(Socket, "A"),
					CC1=decode(Socket, fetch(Socket,[])),
					CC2=lists:droplast(CC),
					make_report(Mark,lists:droplast(CC1),Ip,lists:last(CC1),UserID),
					make_report(Mark,lists:droplast(CC2),Ip,lists:last(CC1),UserID);
					%%From ! {status_out,"find mac : done for "++Ip },
					%%From ! {update_cross_table,UserID};
					
				"Session closed by automatic logout.\r" ->
					From ! {connection_lost},
					From ! {status_out, "find mac : Session closed by automatic logout."};
				
				Desc -> 
					make_report(Mark,lists:droplast(CC),Ip,Desc,UserID) 
			 end,
			 From ! {status_out,"find mac : done for "++Ip },
                         From ! {update_cross_table,UserID},
			 %%From ! {status_out, "find mac : cross-table updated"},
			 manager();
	
		{From, Socket, sh_int_br, Ip, Mark, UserID } ->
			 From ! { status_out,"find up : request to switch "++Ip },
			 ok = gen_tcp:send(Socket, "show interfaces brief\n"),
			 An = fetch(Socket,[]),
			 [_|CC]=decode(Socket, An),
			 case lists:last(CC) of
                                "--- [Space] Next page, [Enter] Next line, [A] All, Others to exit ---" ->
                                        %%From ! { status_out,"find up : request to switch "++Ip },
					ok  = gen_tcp:send(Socket, "A"),
                                        [_|[_|CC0]] = lists:droplast(CC),
					CC1 = decode(Socket, fetch(Socket,[])),
                                        make_port_brief_report(Mark,CC0,Ip, UserID),
                                        make_port_brief_report(Mark,lists:droplast(CC1),Ip, UserID);
					%%From ! {status_out, "find up : READY FOR A NEXT HOP! "++Ip},
					%%From ! {update_cross_table,UserID};
					%%spawn(find_port_via_mac,read_cross,[]);
                                
				"Session closed by automatic logout.\r" ->
                                        From ! {connection_lost},
					From ! {status_out, "Session closed by automatic logout."};
                                
				_Smtnels ->
					%%ok  = gen_tcp:send(Socket, "A"),
                                        [_|[_|CC0]] = lists:droplast(CC),
                                        %%CC1 = decode(Socket, fetch(Socket,[])),
                                        make_port_brief_report(Mark,CC0,Ip, UserID),
                                        %%make_port_brief_report(Mark,lists:droplast(CC1),Ip, UserID),
                                        From ! {status_out, "find up : cross-tables updated."} 
					%%From ! {status_out, "find up : it's a kind of magic"}
                         end,
			 From ! {status_out, "find up : READY FOR A NEXT HOP! "++Ip},
                         From ! {update_cross_table,UserID},
			 manager();
		_Other ->
			 io:format("status_out telnet manager : unexpected event is called",[])
	end.

connect(Target) ->
	gen_tcp:connect(Target, 23, [list, inet, {packet, raw}, {active, false}]).

make_port_brief_report_ins(ID,Str,UserID,Mark,Da,Ti)->
	
	case {db_suite:get_from(cross_actions,ID), db_suite:get_from(port_desc_string,ID)} of
		{[], []} ->
			 db_suite:insert_mark(ID,Str,["None::"],'FIRST_CONN',Da,Ti,{0,0,0},{0,0,0},UserID);
		
		{[{cross_actions,_,[{test,{Y,M,D},{H,Min,S}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m}] , [{port_desc_string,_,Desc_c,_}]} ->
			case {Da,Ti, db_suite:get_from(cross_mark,ID)} of
				{{Y,M,D},_,  [{cross_mark,_,_,jump}]} ->
					do_nothing;
				{{Y,M,D},{H,Min1,S1},  [{cross_mark,_,_,_}]} -> 
					T=(Min1*60+S1)-(Min*60+S),
					if 
						T < 21, 
						Desc_c /= Str ->
							db_suite:insert_mark(ID,Str,Mark,jump,{Y,M,D},{H,Min1,S1},{Y,M,D},{H,Min1,S1},UserID);
						true -> 
                                                      	db_suite:add_action(ID,[{test,{Y,M,D},{H,Min1,S1}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m)
					end;
				_els->
                                        db_suite:add_action(ID,[{test,Da,Ti},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m)
			end;
		%%{[{cross_actions,_,[_,{mark,Da,_}],_}] , [{port_desc_string,_,_,jump}]} ->
		%%	do_nothing;
		%%{[{cross_actions,{test,Da,{H,Min,S}},[_,{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m}] , [{port_desc_string,_,Desc_c,_}]} ->
		%%	if
                %%                Desc_c /= Str ->
		%%			db_suite:insert_mark(ID,Str,Mark,jump,Da,Ti,Da,Ti,UserID);
                %%                true ->
                %%                        db_suite:add_action(ID,[{test,Da,Ti},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m)
                %%        end;
		%%{[{cross_actions,{test,{Y,M,D},{H,Min,S}},[_,{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m}] , [{port_desc_string,_,Desc_c,_}]} ->
		%%	db_suite:add_action(ID,[{test,Da,Ti},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m);
                	
		%%{[_] , []} ->
		%%	db_suite:insert_mark(ID,Str,["Empty::"],'FIRST_CONN',Da,Ti,{0,0,0},{0,0,0},UserID);
		Else -> 
			db_suite:insert_mark(ID,Str,["Empty::"],'FIRST_CONN',Da,Ti,{0,0,0},{0,0,0},UserID),
			io:format("[ Else ^ ~p , ]",[Else])
	end.

make_port_brief_report(_,[],_,_)->
	ok;
make_port_brief_report(Mark,[Str|Text],Ip,UserID) -> 	
	case string:tokens(Str," ") of
		[_|["1/"|[B|List]]] ->  Port= "1/ "++B;
		[_|[_|["1/22"|List]]] -> Port = "1/22";
		[_|[P|List]] -> Port=P
	end,
	Date=date(),
	Time=time(),
	make_port_brief_report_ins(Ip++":"++Port,List,UserID,Mark,Date,Time), 
 	make_port_brief_report(Mark,Text,Ip,UserID).

make_report(_,[[]],_,_,_)->
		ok;
make_report(_,[],_,_,_)->
		ok;
make_report(Mark,[Str|Text],Target,Desc,UserID) ->
	case string:tokens(Str, " ") of
		[_,Port,Mac,_,_] ->
			Port1=Port;
		[_,Dev,Port,Mac,_,_] ->
			Port1=Dev++" "++Port;
		_SmtnEls -> Port1="Unkwn", Mac=Port1
	end,
	ID=Target++":"++Port1,
	case Mark of
		"Link?" -> 
			Comm = 'AUTO_LINK';
		_smtnels -> 
			Comm = via_mac
	end,
			db_suite:insert_mark(ID,Mark,Comm,date(),time(),date(),time(),UserID),
			db_suite:add_mac(ID,Mac,Comm),
	make_report(Mark,Text,Target,Desc,UserID).

disconnect(Socket)->
	ok = gen_tcp:send(Socket, "configure\nmac-address-table aging-time 300\n"),
	ok = gen_tcp:send(Socket, "end\nexit\n"),
	case gen_tcp:close(Socket) of
		ok    ->  
			io:format("status_out, disconnect :  connection terminated",[]);
		_Err -> 
			io:format("status_out, disconnect :  connection terminated ERROR!",[])
	end.

decode(Sock, Bs)->
	case Bs of
		%%handshaking/negotiation sequence
		{ok, [[], 255, 253, 24]} -> 					
		%%IAC DO TERMINAL-TYPE 
					ok = gen_tcp:send(Sock, [255, 251, 24]),
				        Answer = fetch(Sock,[]),
					decode(Sock, Answer);
					%%IAC WILL TERMINAL-TYPE
					
		{ok, [[], 255, 250, 24, 1, 255, 240]} -> 				
		%%IAC SB TERMINAL-TYPE SEND IAC SE
					ok = gen_tcp:send(Sock, [255, 250, 24, 0, 65, 78, 83, 73, 255, 240]),
					Answer = fetch(Sock,[]),
					decode(Sock, Answer);
					%%IAC SB TERMINAL-TYPE IS "ANSI" IAC SE
		%%some other commands
		{ok, ANY} ->		
					re:split(ANY,"[\n]",[{return,list},trim])
	end.

%%fetch data from socket
fetch(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0, 1000) of
        {ok, B} ->
            fetch(Sock, [Bs|B]);
        {error, closed} ->
		{ok, Bs};
	{error, timeout} ->
		%% all messeges has been received
		{ok, Bs}
    end.
