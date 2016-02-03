-module(find_port_via_mac).
-compile(export_all).
%%-include_lib("kvs/include/entry.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
	case wf:user() /= undefined of %%user recognize  
    		true  -> 
			 [{delegate,_, User_role}] = db_suite:get_from(delegate,wf:user()),
			 [U|_]=User_role,
			 wf:session(user_role,U),
			 %%io:format("~p",[wf:session(user_role)]),			 
			 #dtl{
				file = "find_port_via_mac",
			      	app=review,
			      	bindings=[{title,<<"модуль сверки | ИОН"/utf8>>},{body,locker(body(),[],wf:session(user_role))}]
			     }; %%rendering page
    		false -> wf:redirect("login1")
	end.


rights_list_up([],_)->
	%%io:format("~p",["OK"]),
	ok;

rights_list_up([{User_Role,block}|_],User_Role)->
	%%io:format("[~p]",[{User_Role,block}]),
	block;

rights_list_up([_|Rest],User_Role)->
	%%io:format("'' ~p ''",[{H,User_Role}]),
	rights_list_up(Rest,User_Role).

locker(List,User)->
	case db_suite:get_from(delegate,User) of
                [{_,_,[U_R|_]}]->
			User_Role=U_R;
			%%io:format(" ~p ",[U_R]);
		[]->
			User_Role=super1
	end,
	locker(List,[],User_Role).

locker([],Answer,_)->
	%%io:format("~p",[lists:flatten(Answer)]),
	Answer;

locker([{Item,ID}|Ist],Answer,User_Role)->
	%%io:format("99~p99",[User_Role]),
	case db_suite:get_from(element,ID) of
		[{element,_,Right_list,_}]->
			case rights_list_up(Right_list,User_Role) of
                		ok   ->
					%%io:format("ID:  ~p",[ID]),
					Answer1= [Answer,Item];
					%%locker(Ist,[Answer,Item],User_Role);
				block -> 
					Answer1= Answer;
                		_ ->
					Answer1= Answer
					%%io:format("~p",[banana])
        		end;
		[] ->	
			Answer1= [Answer,Item];
			%%locker(Ist,[Answer,Item],User_Role);
		_  -> 
			Answer1=Answer
			%%io:format("~p ~p",[Els, {ID}]),do_nothing 
	end,
	locker(Ist,Answer1,User_Role);

locker([L|Ist],Answer,User_Role)->
	locker(Ist,[Answer,L],User_Role).

body() ->
	[
		 #panel{ class= <<"row"/utf8>>, body= locker([

                        {#panel{ class= <<"cols col-3"/utf8>>, body= [
                                                                <<"Изменить Маркер<br>"/utf8>>,
                                                                #textbox{id=mark_id, value = <<"Текущий ID порта"/utf8>>}
                                                                ]},change_mark},
                        {
                        #panel{ class= <<"cols col-3"/utf8>>, body= [
                                                                <<"Маркер<br>"/utf8>>,
                                                                #textbox{id=mark, value = <<"1-1-1(1-2)">>}
                                                                ]},marker},
                        {
                        #panel{ class= <<"cols col-3"/utf8>>, body=
                                                                [
                                                                <<"Следующий <br>"/utf8>>,
                                                                #button{
                                                                        id=next_mark,
                                                                        body= "o-0-0(O-o)" ,
                                                                        postback=get_next_mark,
                                                                        source=[mark]
                                                                        }
                                                                ]
                                                                },next_marker},
                        {
                        #panel{class= <<"cols col-3"/utf8>>, body=
                                                                #button{
                                                                        id=wrong_mark,
                                                                        body= <<"Удалить текущий маркер"/utf8>>,
                                                                        postback=wrong
                                                                }},del_marker}

                ],wf:user())},	
		{#panel{ class= <<"row"/utf8>>, body=[
                        #panel{class= <<"cols col-12full"/utf8>>, body=
                                                               	#panel{id=new_content, body=["last update view"]}
                                                                }
                ]},new_content},
		{
                #panel{ class= <<"row"/utf8>>, body=[
                        #panel{ class= <<"cols col-6"/utf8>>, body= [
                                                                #panel{id=label2, body = <<"MAC адрес устройства: <br>"/utf8>>},
                                                                #textbox{id=mac, value = <<"00-00-DB-00-00-DB">>}
                                                                ]},
                        #panel{ class= <<"cols col-6"/utf8>>, body=
                                                                #button{
                                                                        id=find,
                                                                        body= <<"Поиск MAC адреса"/utf8>>,
                                                                        postback=finding,
                                                                        source=[mac,ip,mark,mark_id]
                                                                }}
                ]},find_mac},
                {
                #panel{ class= <<"row"/utf8>>, body=[
                        #panel{ class= <<"cols col-12full"/utf8>>, body=
                                                                #button{
                                                                        id=find_up_port,
                                                                        body= <<"Поиск изменений в описании"/utf8>>,
                                                                        postback=finding_up_port,
                                                                        source=[ip,mark,mark_id]
                                                                }}
                ]},change_find},

		{
                #panel{ class= <<"row"/utf8>>, body=[
			
                        #panel{id=label1, class= <<"cols col-6resize"/utf8>>, body= [
                                                                <<"Список IP адресов : "/utf8>>,
                                                                #panel{id=search_list, body =
                                                                <<"[Список сейчас пуст]"/utf8 >>}
                                                                ]},
                        #panel{class= <<"cols col-6resize"/utf8>>, body=
                                                                #panel{id=status_bar, body=[ <<"Текущее состояние:"/utf8>> ]}
                                                                }
                ]},list_of_add_and_status},
		#panel{ class= <<"row"/utf8>>, body= locker([
			{#panel{id=label0, class= <<"cols col-3"/utf8>>, body= [ <<"Пользователь : <br>"/utf8>>] ++ wf:user()},{mp,user_bar}},
			#panel{ class= <<"cols col-3"/utf8>>, body=
                                                                #button{
                                                                        id=connect,
                                                                        body= <<"Подключить"/utf8>>,
                                                                        postback=connect
                                                                }},
			#panel{class = <<"cols col-3"/utf8>>, body= [
                                                                        <<"IP адрес <br>"/utf8>>,
                                                                        #textbox{id=ip_list,value = <<"192.168.20.44">>}
                                                                ]},
			{#panel{ class= <<"cols col-3"/utf8>>, body=
                                                                #button{
                                                                        id=logout,
                                                                        body= <<"Выход"/utf8>>,
                                                                        postback=logout
                                                               }},{mp,exit_button}}

		],wf:user())},
		{
		#panel{ class= <<"row"/utf8>>, body= [
			#panel{class = <<"cols col-6"/utf8>>, body= 
								#button{
                                                                        id=add_ip,
                                                                        body= <<"Доб. IP адрес"/utf8>>,
                                                                        postback=add_ip,
                                                                        source=[ip_list]
                                                                }},
			#panel{class = <<"cols col-6"/utf8>>, body=
								#button{
                                               				id=del_ip,
                                                			body= <<"Удалить IP адрес"/utf8>>,
                                                			postback=del_ip,
                                                			source=[ip_list]
                                        			}}
		]},add_ip_del_ip },
		{
		#panel{ class= <<"row"/utf8>>, body=[
			#panel{class= <<"cols col-12full"/utf8>>, body=
                                        			#panel{id=db_out,  body=["cross-connect table"]}
								}
		]},cross_table},
		 {
                #panel{ class= <<"row"/utf8>>, body=[
                        #panel{class= <<"cols col-12full"/utf8>>, body=
								 #link{class=["carousel-control", left], url="help", body= <<"<h2>Руководство</h2>"/utf8>>}}
                ]},help_link}
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
		
		{update_cross_table,UserID}->
			%%io:format("~p ", [UserID]),
			{L,La}=read_cross(UserID),
			%%io:format("~p ~p ~p", [UserID, L, La]),
			wf:update(new_content, #panel{id=new_content, body=[#p{body= La }]}),
                        wf:update(db_out, #panel{id=db_out, body=[#p{body= L }]}),
			wf:flush(list_to_atom(UserID)),
                        listener(list_to_atom(UserID));
		
		_Other ->
			some_other
	end.
read_cross()->
	read_cross(wf:user()).
read_cross(UserID)->
	case db_suite:get_from(account,UserID) of
		 [{account,_,_,[Conn_List|[_|_]]}] ->
				%%io:format(" ~p ",[Conn_List]),
                                read_cross(Conn_List,[],[],UserID);
                 _els -> do_nothing
	end.
read_cross([],L,La,_)->
	{L,La};

read_cross([TargetTuple|Rest],L,La,User)->
	{Target,_,_,_}=TargetTuple,
	[{listing,port_edge_core28,PortList,standard_switch_port}]=db_suite:get_from(listing, port_edge_core28),
	{L1,La1}=read_cross(Target,PortList,[],[],User),
	read_cross(Rest,[L1|L],[La1|La],User).

read_cross(Target,[],List,Last,User)->
	Row = locker([<<"<div class= row>
                                <div class= 'cols col-2asis' >
                                        ID Порта
                                </div>"/utf8>>,
				{
                                <<"<div class= 'cols col-3asis' >
                                        Описание
                                </div>"/utf8>>,cross_tab_desc_col},
                                <<"<div class= 'cols col-2asis'>
                                        Маркер
                                </div>"/utf8>>,
				{
                                <<"<div class= 'cols col-2asis'>
                                        MAC-Адрес
                                </div>"/utf8>>,cross_tab_mac_col},
                                {
				<<"<div class= 'cols col-1asis'>
                                        Последний Доступ
                                </div>"/utf8>>,cross_tab_last_access_col},
                                <<"<div class= 'cols col-2asis'>
                                        Время Маркера
                                </div>"/utf8>>,
                                {
				<<"<div class= 'cols col-1asis'>
                                        Пользователь
                                </div>"/utf8>>,cross_tab_user_col},
				{
                                <<"<div class= 'cols col-1asis'>
                                        Комментарий
                                </div>"/utf8>>,cross_tab_comments_col},
        "</div>"],User),
        {[Row|List
	],[
	<<"<div class= row> <div class = 'cols col-12full'> последние изменения в"/utf8>>]++Target++[<<" </div><div/>"/utf8>>,Row|Last

		]};

read_cross(Target,[Port|Ports],List,Last,User)->
        case db_suite:get_from(cross_actions,Target++":"++Port) of
                [{cross_actions,ID,
                [{test,{Yt,Mt,Dt},{Ht,Mit,St}},{mark,{Ym,Mm,Dm},{Hm,Mim,Sm}}],UserID}] ->
                        [{port_desc_string,_,Desc_Str,_}]=db_suite:get_from(port_desc_string,ID),
                        [{cross_mark,_,Mark,Comment}]=db_suite:get_from(cross_mark,ID),
                        case db_suite:get_from(cross_mac,ID) of
                                [] ->
                                        Mac="not marked with mac";
                                [{cross_mac,_,Mac,_}] ->
                                        ok;
                                _Els ->
                                        Mac="unk mac!"
                        end,
                        String=locker(["<div class=row><div class ='cols col-2asis'>"++ID++"</div>",
			{"<div class ='cols col-3asis'>"++lists:concat(Desc_Str)++"</div>",cross_tab_desc_col},
			"<div class ='cols col-2asis'>"++lists:concat(Mark)++"</div>",
			{"<div class ='cols col-2asis'>"++Mac++"</div>",cross_tab_mac_col},
			{"<div class ='cols col-1asis'>"
                        ++integer_to_list(Dt)++"."++integer_to_list(Mt)++"."++integer_to_list(Yt)++"  "
                        ++integer_to_list(Ht)++":"++integer_to_list(Mit)++":"++integer_to_list(St)++"</div>",cross_tab_last_access_col},
			"<div class ='cols col-2asis'>"
                        ++integer_to_list(Dm)++"."++integer_to_list(Mm)++"."++integer_to_list(Ym)++"  "
                        ++integer_to_list(Hm)++":"++integer_to_list(Mim)++":"++integer_to_list(Sm)++"</div>",
			{"<div class ='cols col-1asis'>"++UserID++"</div>",cross_tab_user_col},
			{"<div class ='cols col-1asis'>"++
                        case is_atom(Comment) of
                                true -> atom_to_list(Comment);
                                false -> Comment
                        end++
                        "</div>",cross_tab_comments_col},
			"</div>"],User),
                        case {date(),time()} of
                        {{Ym,Mm,Dm},{Hm,Mim,_}}->
                                Last1=[String|Last],
                                case {db_suite:get_from(account,User), Comment} of
                                        {_, "Wrong"} ->
                                                         do_nothing;
                                        {[{account,_,Pass,[Conn_List,Wrong_List]}], _} ->
                                                         db_suite:add_account(User,Pass,[Conn_List,[ID|Wrong_List]]);
                                        _els ->
                                                do_nothing
                                end;
                                _Else ->
                                        Last1=Last
                        end;
                []      ->
                                String="<div class=row><div class ='cols col-1asis'>"++Target++":"++Port++"<div/><div class ='cols col-7asis'>[NONE]</div></div>",
                                Last1=Last;
                _Else   ->
                                String="<div class=row><div class ='cols col-1asis'>"++Target++":"++Port++"<div/><div class ='cols col-7asis'>[Read Error!]</div></div>",
                                Last1=Last
        end,
        read_cross(Target,Ports,[String|List],Last1,User).



%% creates connection list
connection_list([]) ->
		case db_suite:get_from(account,wf:user()) of 
			[{account,_,Pass,_}] ->
				db_suite:add_account(wf:user(),Pass,[wf:session(conn_list),[wrong_list]]);
			_els -> do_nothing 
		end,
		wf:update(search_list, #panel{id=search_list, body= [["<br>"|TargetList] || {TargetList,_,_,_} <- wf:session(conn_list)] }),
                wf:flush(list_to_atom(wf:user()));

connection_list([H|Rest]) ->
	Pid = spawn(telnet_client,manager,[]),%%spawn listener and manager processes
	PidL = spawn(?MODULE,listener,[list_to_atom(wf:user())]),
	Pid ! {self(),PidL,conn_login,H,wf:user()}, %% send conn_login to manager 
	
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

find_wrong([])->
	ok;

find_wrong([CurrID|List])->
	case db_suite:get_from(cross_mark,CurrID) of
                [] ->
                        do_nothing;
		[{cross_mark,_,_,"Wrong"}] ->
			do_nothing;
                [{cross_mark,_,Old_Mark,_}] ->
                        case Old_Mark of
                                [A,B,C] -> 
                                        NewMark = ["WRONG_"++A,B,C];
                                [A|B] ->
                                        NewMark = ["WRONG"++A,B]
                        end,
			db_suite:add_action(CurrID,[{test,date(),time()},{mark,date(),time()}],wf:user()),
        		db_suite:add_mark(CurrID, NewMark, "Wrong");
		Else -> 
			io:format("elsewrong ~p",[Else])
        end,
	find_wrong(List).

%%event handlers
event(init) ->
	wf:reg(list_to_atom(wf:user()));

event(wrong) ->
	case db_suite:get_from(account,wf:user()) of
        	[{account,_,Pass,[Conn_List,Wrong_List]}] ->
                	find_wrong(Wrong_List),
			db_suite:add_account(wf:user(),Pass,[Conn_List,[wrong_list]]),
			{L,La}=read_cross(wf:user()),
                        wf:update(new_content, #panel{id=new_content, body=[#p{body= La }]}),
                        wf:update(db_out, #panel{id=db_out, body=[#p{body= L }]}),
                        wf:flush(list_to_atom(wf:user()));
                _els -> 
			do_nothing
        end;

event(logout) ->
	case wf:session(conn_list) of
                undefined ->
                        ok;
                [] ->
                        ok;
                _Else ->
                        [ {From ! {stop, Sock}, Pidl ! {stop}} || {_,From,Pidl,Sock} <- wf:session(conn_list)],
                        wf:session(conn_list,[])
        end;

event(get_next_mark) ->
	wf:update(mark, #textbox{id=mark, value = db_suite:next_mark([".",".",".",".","."],wf:q(mark))});

event(connect) ->
	case db_suite:get_from(account,wf:user()) of
                [{account,_,Pass,_}] ->
                        db_suite:add_account(wf:user(),Pass,[conn_list,[wrong_list]]);
                _els -> do_nothing
        end,

	case wf:session(conn_list) of
		undefined ->
			ok;
		[] ->
			ok;
                _Else -> 
			[ {From ! {stop, Sock}, Pidl ! {stop}} || {_,From,Pidl,Sock} <- wf:session(conn_list)],
			wf:session(conn_list,[])
	end,
	connection_list(wf:session(ipl)),
	find_up_port(wf:session(conn_list),"just_connected"),
	find_port(wf:session(conn_list),"CA-01-15-90-00-08","Link?");%%"The Truth Is Out There"

event(finding_up_port) ->
	case db_suite:get_from(account,wf:user()) of
                [{account,_,Pass,[Conn_List|_]}] ->
                        db_suite:add_account(wf:user(),Pass,[Conn_List,[wrong_list]]);
                _els -> do_nothing
        end,

        wf:update(mark_id, #textbox{id=mark_id, value = "Current_ID" }),
        case db_suite:get_from(cross_actions,wf:q(mark_id)) of
                [] ->
			find_up_port(wf:session(conn_list),wf:q(mark));
		
		[{cross_actions,ID,[_,{mark,{Y,M,D},{_,_,_}}],_}]->
                        case date() of
                                {Y,M,D} ->
					db_suite:insert_mark(ID,wf:q(mark),"Manually",{Y,M,D},time(),{Y,M,D},time(),wf:user()),
					{L,La}=read_cross(wf:user()),
                                        wf:update(new_content, #panel{id=new_content, body=[#p{body= La }]}),
                                        wf:update(db_out, #panel{id=db_out, body=[#p{body= L }]}),
                                        wf:flush(list_to_atom(wf:user()));

                                _Else -> ok
                        end;
                _Els -> ok
        end;

event(finding) ->
	case db_suite:get_from(account,wf:user()) of
                [{account,_,Pass,[Conn_List|_]}] ->
                        db_suite:add_account(wf:user(),Pass,[Conn_List,[wrong_list]]);
                _els -> do_nothing
        end,
	wf:update(mark_id, #textbox{id=mark_id, value = "Current_ID" }),
	case db_suite:get_from(cross_actions,wf:q(mark_id)) of
		[] ->	
			%%timer:sleep(3000),
			find_port(wf:session(conn_list),wf:q(mac),wf:q(mark));

		[{cross_actions,ID,[_,{mark,{Y,M,D},{_,_,_}}],_}] -> 
			case date() of
				{Y,M,D} ->
					db_suite:insert_mark(ID,wf:q(mark),"Manually",{Y,M,D},time(),{Y,M,D},time(),wf:user()),
					{L,La}=read_cross(wf:user()),
					wf:update(new_content, #panel{id=new_content, body=[#p{body= La }]}),
					wf:update(db_out, #panel{id=db_out, body=[#p{body= L }]}),
					wf:flush(list_to_atom(wf:user()));
				_Else -> ok
			end;
		_Els -> ok
	end;

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
