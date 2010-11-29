-module(gen_imap).
-author("William King <quentusrex@gmail.com>").
-export([init/0, init/1, loop0/1, loop0/2, loop/1, connection_distributor/2, command_handler/2, run_command/4]).

-record(imap_connection, {port,
			 state="Not Authenticated",
			 timeout=10000).
-define(PORTNO, 143).

%% Initialize the IMAP daemon with default port
init() ->
    init(?PORTNO).

%% Initialize the IMAP daemon with specified port
init(Port) ->
    case gen_tcp:listen(Port, [{active, false}]) of
	{ok, Socket} ->
	    spawn_link(?MODULE, loop, [Socket]);
	Reason ->
	    io:format("Failed to initialize because: ~p~n", [Reason])
    end.

%% Main loop. will spawn worker processes to handle new incoming connections when 
%% it receives a message from a worker process that a new connection was established. 
loop(Socket) ->
    receive
	next_worker ->
	    spawn_link(?MODULE, connection_distributor, [self(), Socket])
    end,
    loop(Socket).

%% Distributor blocks on the accept waiting for new connections. 
%% If it gets a new connection it will tell the main loop to spawn a process to 
%% listen for the next connection.
connection_distributor(Server, Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, Port} ->
	    Pid = self(),
	    io:format("Client opened a connection. ~n", []),
	    Server ! next_worker, %% If we get a connection, spawn off a new worker while we handle this one.
	    gen_tcp:send(Port, "* OK gen_imap server ready\n"),
	    command_handler(Port),
	    gen_tcp:send(Port, "* BYE gen_imap server closing connection\n");
	Reason ->
	    Server ! next_worker,
	    io:format("Unable to accept socket because: ~p~n", [Reason])
    end.

%% Will stay with the connection for the life of the connection. 
%% Has a default timeout of 10 seconds which will close the connection if there is no activity.
%% Timeout can be set to 0 on the call, this will be required to support IDLE command, for fake push notification.
command_handler(State) when is_record(State, imap_connection) ->
    case gen_tcp:recv(State#imap_connection.port, 0, State#imap_connection.port) of
	{ok, Packet} ->
	    [Ident | Commands] = string:tokens(Packet, " "),
	    [Command | Args ] = Commands,
	    %% Should really spawn a run_command so that heavy commands do not slow down the command input.
	    run_command(string:to_upper(Command), State, Ident, Args),
	    command_handler(State);
	{error, closed} ->
	    io:format("Client closed the connection. ~n", []);
	Reason ->
	    io:format("Unable to recv from socket because: ~p~n", [Reason])
    end;
command_handler(Port) ->
    command_handler(Port, 10000);
command_handler(Port, Timeout) ->
    State = #imap_connection{port=Port, timeout=Timeout},
    command_handler(State).

run_command("CAPABILITY", State, Ident, Args) ->
    gen_tcp:send(State#imap_connection.port, "* CAPABILITY NOOP LOGOUT IDLE\n"),
    gen_tcp:send(State#imap_connection.port, Ident ++ " OK CAPABILITY completed\n";
run_command("IDLE", State, Ident, Args) ->
    State#imap_connection.timeout=0,
    command_handler(State);
run_command("NOOP", State, Ident, Args) ->
    command_handler(State);
run_command("LOGOUT", State, Ident, Args) ->
    State#imap_connection.state="Not Authenticated",
    gen_tcp:send(Port,  Ident ++ " OK LOGOUT completed"),
    {ok, closed};
run_command(Command, State, Ident, Args) ->
    gen_tcp:send(Port, "BAD parse error: " ++ Command ++ "command not implemented\n").
