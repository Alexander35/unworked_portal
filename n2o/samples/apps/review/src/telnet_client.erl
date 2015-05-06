-module(telnet_client).
-export([connect/1, disconnect/1, fetch/2, manager/0]).

%%interface manager
manager()->
	receive	
		%%connecting and logginig sequence starts here
		{From,PidL, conn_login, Target} -> 
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
                                       		                		C=decode(Socket, An),
										ok = gen_tcp:send(Socket,"configure\nmac-address-table aging-time 10\nend\n"),
										fetch(Socket,[]),
										PidL ! {status_out,"connected"},
										From ! {self(),PidL,socket,Socket,Target},
										PidL ! {update_cross_table};

									_Any -> 
										From ! {self(),PidL,no_connect,Target}
                                        			end,
								manager();
							false -> 
								From ! {self(),PidL,no_connect,Target}
								%%everything else we thinks that connection is not established 
						end;

                			{error, _} -> 
							From ! {conn_error, Target};
							%%if Target not reachable or not exist
					_Other -> 
							other
				end;
						
			
		{stop, Sock} ->
			 disconnect(Sock);
		
		{From, Socket, sh_mac, Cmd, Ip, Mark, UserID} ->
			 ok = gen_tcp:send(Socket, Cmd),
                         An = fetch(Socket,[]),
			 [_|CC]=decode(Socket, An),%% cutting first element == command show mac-a
			
			 case lists:last(CC) of
				"--- [Space] Next page, [Enter] Next line, [A] All, Others to exit ---" ->
					From ! {status_out, "find mac : update cross-tables"}, 
					ok = gen_tcp:send(Socket, "A"),
					CC1=decode(Socket, fetch(Socket,[])),
					CC2=lists:droplast(CC),
					make_report(Mark,lists:droplast(CC1),Ip,lists:last(CC1),UserID),
					make_report(Mark,lists:droplast(CC2),Ip,lists:last(CC1),UserID);
					
				"Session closed by automatic logout.\r" ->
					From ! {connection_lost};
				
				Desc -> 
					make_report(Mark,lists:droplast(CC),Ip,Desc,UserID) 
			 end,

			 From ! {status_out, "find mac : cross-table updated"},
			 From ! {update_cross_table},
			 manager();
		
		{From, Socket, sh_int_br, Ip, Mark, UserID } ->
			 ok = gen_tcp:send(Socket, "show interfaces brief\n"),
			 An = fetch(Socket,[]),
			 [_|CC]=decode(Socket, An),
			 case lists:last(CC) of
                                "--- [Space] Next page, [Enter] Next line, [A] All, Others to exit ---" ->
					From ! {status_out, "find up : update cross-tables"},
                                        ok  = gen_tcp:send(Socket, "A"),
                                        [_|[_|CC0]] = lists:droplast(CC),
					CC1 = decode(Socket, fetch(Socket,[])),
                                        make_port_brief_report(Mark,CC0,Ip, UserID),
                                        make_port_brief_report(Mark,lists:droplast(CC1),Ip, UserID),
					From ! {status_out, "find up : cross-tables updated. push button again"},
					From ! {update_cross_table};
					
                                "Session closed by automatic logout.\r" ->
                                        From ! {connection_lost};

                                _Smtnels -> 
					ok
                         end,
			 manager();
	
		_Other ->
			 other
		
				

	end.

connect(Target) ->
	gen_tcp:connect(Target, 23, [list, inet, {packet, raw}, {active, false}]).

make_port_brief_report_ins(ID,Str,UserID,Mark)->
	case {db_suite:get_from(cross_actions,ID), db_suite:get_from(port_desc_string,ID)} of
		{[], []} ->
			 db_suite:add_port_str(ID,Str,no_comm),
                         db_suite:add_action(ID,[{test,date(),time()},{mark,none,none}],UserID),
			 db_suite:add_mark(ID, "None", no_comm);
		
		{[{cross_actions,_,[{test,{Y,M,D},{H,Min,S}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m}] , [{port_desc_string,_,Desc_c,_}]} ->
			case {date(),time()} of
				{{Y,M,D},{H,Min1,S1}} -> 
					T=(Min1*60+S1)-(Min*60+S),
					if 
						T < 11, Desc_c /= Str ->
							%%io:format("~p ~p~n",[ Desc_c, Str]),
							db_suite:add_port_str(ID,Str,no_comm),
							db_suite:add_action(ID,[{test,{Y,M,D},{H,Min1,S1}},{mark,{Y,M,D},{H,Min1,S1}}],UserID),
							db_suite:add_mark(ID, Mark, no_comm);
						true -> 
                                                        db_suite:add_action(ID,[{test,{Y,M,D},{H,Min1,S1}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],User_m)
					end;
				{{Y1,M1,D1},{H1,Mi1,Se1}}->
                                        db_suite:add_action(ID,[{test,{Y1,M1,D1},{H1,Mi1,Se1}},{mark,{Y,M,D},{H,Min,S}}],User_m)
			end;
		Else->
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
	make_port_brief_report_ins(Ip++":"++Port,List,UserID,Mark), 
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
	%%case db_suite:get_cross(Target++":"++Port1) of
	%%	[{cross,_,"Link?",_,_,_,_}] ->
	%%		next;
	%%	_Else -> 
			db_suite:add_mac(ID,Mac,no_comm),
                        db_suite:add_action(ID,[{test,date(),time()},{mark,date(),time()}],UserID),
                        db_suite:add_mark(ID, Mark, no_comm),
	%%		db_suite:add_cross(time(),date(), Mark, Desc, Target, Port1, Mac, no_comment)
	%end,
	make_report(Mark,Text,Target,Desc,UserID).

disconnect(Socket)->
	ok = gen_tcp:send(Socket, "configure\nmac-address-table aging-time 300\n"),
	ok = gen_tcp:send(Socket, "end\nexit\n"),
	case gen_tcp:close(Socket) of
		ok    -> {ok, conn_term};
		Error -> {error, Error}
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
