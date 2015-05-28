-module(db_suite).
-compile(export_all).

-record(account, {name, pass, comment}).
-record(port_desc_string, {target_port,desc_string, comment}).
-record(cross, {target_port,mark, work_date, desc, mac, comment}).
-record(cross_actions, {target_port,work_date, comment}).
-record(cross_mark, {target_port,mark, comment}).
-record(cross_mac,{target_port,mac,comment}).

create_cross_tab() ->
	mnesia:create_table(cross, [{attributes, record_info(fields, cross)}, {disc_copies, [node()]}]). 

init_port_desc_string()->init(port_desc_string, record_info(fields, port_desc_string)).
init_cross_actions()->init(cross_actions, record_info(fields, cross_actions)).
init_cross_mark()->init(cross_mark, record_info(fields, cross_mark)).
init_cross_mac()->init(cross_mac, record_info(fields, cross_mac)).

init(TabName,Fields)->
	mnesia:create_table(TabName, [{attributes, Fields }, {disc_copies, [node()]}]).
init()->
	mnesia:create_table(account, [{attributes, record_info(fields, account)}, {disc_copies, [node()]}]).

next_mark(_,[_|[]])->
	ok;

next_mark(List,[])->
	case List of
		[X,X1,X2,11,3] -> next_mark([X,X1,X2+1,1,2],[]);
		[X,X1,3,5,6]   -> next_mark([X,X1+1,1,1,2],[]);
		[X,10,1,1,2]   -> next_mark([X+1,1,1,1,2],[]);
		[10,1,1,1,2]   -> "Off! =(";
		[X,X1,X2,Y,Y1] -> integer_to_list(X)++"-"
					++integer_to_list(X1)++"-"
						++integer_to_list(X2)++"("
							++integer_to_list(Y)++"-"
								++integer_to_list(Y1)++")";
		Else -> {okay,Else}
	end;

next_mark(List,[H|[_|N]])->
	case List of
		[".",".",".",".","."] -> next_mark([H-48,".",".",".","."],N);
		[ X ,".",".",".","."] -> next_mark([X,H-48,".",".","."],N);
		[ X , X2,".",".","."] -> next_mark([X,X2,H-48,".","."],N);
		[ X , X1, X2,".","."] -> next_mark([X,X1,X2,H-48,"."],N);
		[ X , X1 ,X2, Y ,"."] -> Yn=Y+2, Y1n=H-46, next_mark([X,X1,X2,Yn,Y1n],[]);
		Else-> {okay,Else}
	end.

add_into(Item)->
	F = fun() ->
                mnesia:write(Item)
        end,
        mnesia:activity(transaction, F).

get_from(TabName,Target)->
		
	F = fun() ->
                mnesia:read(TabName, Target)
        end,
        mnesia:activity(transaction, F).

add_mac(ID, Mac, Comm)->
        add_into(#cross_mac{target_port=ID,mac=Mac,comment=Comm}).

add_port_str(ID, Desc, Comm)->
	add_into(#port_desc_string{target_port=ID, desc_string=Desc, comment=Comm}).

get_port_str(ID)->
	get_from(port_desc_string,ID).

insert_mark(ID,Mark,Comm,DTT,TTT,DMT,TMT,UserID)->
        insert_mark(ID,"hold string",Mark,Comm,DTT,TTT,DMT,TMT,UserID).

insert_mark(ID,Str,Mark,Comm,DTT,TTT,DMT,TMT,UserID)->
	case db_suite:get_from(cross_mark,ID) of
                [] ->
                        NewMark = [Mark++"::"];
                [{cross_mark,_,Old_Mark,_}] ->
                         case Old_Mark of
				[A] ->
					NewMark = [Mark++"::",A];
				[A,B]->
					NewMark = [Mark++"::",A,B];
                                [A,B,C] ->
                                        NewMark = [Mark++"::",A,B];
                                _Els ->
					io:format(" ~p ",[Old_Mark]),
                                        NewMark = [Mark++"::",Old_Mark]
                        end
        end,
	case Str of
		"hold string"->
			do_nothing;
		_els->
			db_suite:add_port_str(ID,Str,Comm)
	end,           
        db_suite:add_action(ID,[{test,DTT,TTT},{mark,DMT,TMT}],UserID),
        db_suite:add_mark(ID, NewMark, Comm).

add_mark(ID, Mark, Comm) ->
	add_into(#cross_mark{target_port=ID, mark=Mark, comment=Comm}).

add_action(ID, Work_date, Comm) ->
	add_into(#cross_actions{target_port=ID, work_date=Work_date, comment=Comm}).

add_cross(Work_time,Work_date,Mark,Desc,ID,Mac,Comment)->
	{H,Min,S}=Work_time,
        {Y,M,D}=Work_date,
	add_into(#cross{target_port=ID,mark=Mark, work_date={Y,M,D,H,Min,S}, desc=Desc, mac=Mac, comment=Comment}).

add_cross(Work_time,Work_date,Mark,Desc,Target,Port,Mac,Comment)->
	{H,Min,S}=Work_time,
	{Y,M,D}=Work_date,
	F = fun() ->
                mnesia:write(#cross{target_port=Target++":"++Port,
				 work_date={Y,M,D,H,Min,S}, mark=Mark, desc=Desc, mac=Mac, comment=Comment})
        end,
        mnesia:activity(transaction, F).

get_cross(Target) ->
        F = fun() ->
                mnesia:read(cross, Target)
        end,
        mnesia:activity(transaction, F).

add_account(Name, Pass, Comment) ->
	F = fun() ->
		mnesia:write(#account{name=Name, pass=Pass, comment=Comment})
	end,
	mnesia:activity(transaction, F).

get_row(Name) ->
	F = fun() ->
		mnesia:read(account, Name)
	end,
	mnesia:activity(transaction, F).

key_test(Name,Pass) ->
	case get_row(Name) of
	     [{account, Name, Pass, Comment}] -> {ok, Name, Comment};
	     [] -> {not_found, Name};
	     _ -> error
	end.
