-module(gen_telnet).
-author('alexanderivanov').
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-export([start_link/1]).
-export([call/1, acall/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/1]).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

terminate(Name)->
	gen_server:cast(Name,stop).
call(Name) ->
    gen_server:call(Name, message).

acall() ->
    gen_server:cast(?SERVER, message).

init(_Args) ->
    {ok, ini_called}.

%%synch call handler
handle_call(Request, From, State) ->
    {reply, From, state_1}.

%%exit call handler
handle_cast(stop, State)->
	{stop, normal, State},
	exit(self(),normal);

%%some cast call
handle_cast(Request, State) ->
	{noreply, state_1}.







%%-module(telnet_client).
%%-export([connect/1, disconnect/1, fetch/2, manager/1, manager/0, listener/0]).

%%entry point.
%%manager(ini)->
%%        Pid = spawn(?MODULE, manager, []),%% spawn new manager process
%%        PidL = spawn(?MODULE, listener, []),
%%        Pid ! {connect, Target, PidL}; %%send connect signal to process

%%interface manager
%%manager()->
%%	receive	

%%		{From, login, socket} ->ok;
						
			
%%		{connect, Target, PidL} ->
%%				PidL ! {self(),connect_stats, connect(Target)}; %% connecting to target
			
%%		Other -> {some_other, Other}		

%%	after 4000 ->
%%		manager()
	
%%	end.
	%%manager()%%loop.

%%GUI listener
%%listener()-> 
%%	receive	
%%		{From, connect_stats, {ok, Socket}} ->
%%				From ! {self(), login, socket};%% connected

%%		{From, connect_stats, {error, Reason}} -> 
%%				"connection: an error occured =(";
%%		Other -> {some_other, Other}

%%        after 4000 ->
%%		listener()  
%%	end.
	%%listener().

%%connect(Target) ->

%%	gen_tcp:connect(Target, 23, [list, inet, {packet, raw}, {active, false}]).%% of
		%%{ok, Socket} ->	Answer = fetch(Socket, []),
				%%decode(Socket, Answer),
				%%disconnect(Socket);
		%%{error, Reason} -> {"Unable to connect", Reason}
	%%end.

%%disconnect(Socket)->
%%	case gen_tcp:close(Socket) of
%%		ok    -> {ok, connection_terminated};
%%		Error -> {error, Error}
%%	end.

%%decode(Sock, Bs)->
%%	case Bs of
		%%handshaking/negotiation sequence
%%		{ok, [[], 255, 253, 24]} -> 					
		%%IAC DO TERMINAL-TYPE 
%%					ok = gen_tcp:send(Sock, [255, 251, 24]);
					%%IAC WILL TERMINAL-TYPE
					
%%		{ok,[[], 255, 250, 24, 1, 255, 240]} -> 				
		%%IAC SB TERMINAL-TYPE SEND IAC SE
%%					ok = gen_tcp:send(Sock, [255, 250, 24, 0, 65, 78, 83, 73, 255, 240]);
					%%IAC SB TERMINAL-TYPE IS "ANSI" IAC SE
					
		%%other commands
%%		{ok,ANY}->io:format("~s",[ANY]),
%%			    case io:get_line("") of
%%				"exit\n" ->
%%					 	ok = gen_tcp:send(Sock, "exit\n"),
%%						{ok,Ans} = fetch(Sock,[]),
%%						io:format("~s",[Ans]),
%%						disconnect(Sock),
%%						exit(0);
%%				Cmd	 ->
%%						ok = gen_tcp:send(Sock,Cmd)
%%			    end
		
%%	end,
%%	Answer = fetch(Sock,[]),
%%        decode(Sock, Answer).

%%fetch(Sock, Bs) ->
%%    case gen_tcp:recv(Sock, 0, 1000) of
%%        {ok, B} ->
%%            fetch(Sock, [Bs|B]);
%%        {error, closed} ->
%%		{ok, Bs};
%%	{error, timeout} ->
		%% all messeges has been received
%%		{ok, Bs}
%%    end.
