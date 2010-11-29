-module(gen_imap).
-author("William King <quentusrex@gmail.com>").
-export([init/0, init/1, loop/1, connection_distributor/2, command_handler/1, command_handler/2, run_command/4]).

-record(imap_connection, {port,
			 state="Not Authenticated",
			 timeout=10000}).
-define(PORTNO, 143).

%% Initialize the IMAP daemon with default port
init() ->
    init(?PORTNO).

%% Initialize the IMAP daemon with specified port
init(Port) ->
    case gen_tcp:listen(Port, [{active, false}]) of
	{ok, Socket} ->
	    Pid = spawn_link(?MODULE, loop, [Socket]),
	    Pid ! next_worker;
	Reason ->
	    io:format("Failed to initialize because: ~p~n", [Reason])
    end.

%% Main loop. will spawn worker processes to handle new incoming connections when 
%% it receives a message from a worker process that a new connection was established. 
loop(Socket) ->
    receive
	next_worker ->
	    spawn_link(?MODULE, connection_distributor, [self(), Socket]);
	{ok, _} ->
	    ok;
	Reason ->
	    io:format("Error: ~p~n", [Reason])
    end,
    loop(Socket).

%% Distributor blocks on the accept waiting for new connections. 
%% If it gets a new connection it will tell the main loop to spawn a process to 
%% listen for the next connection.
connection_distributor(Server, Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, Port} ->
	    Server ! next_worker, %% If we get a connection, spawn off a new worker while we handle this one.
	    io:format("Client opened a connection. ~n", []),
	    gen_tcp:send(Port, "* OK gen_imap server ready\n"),
	    command_handler(Port),
	    gen_tcp:send(Port, "* BYE gen_imap server closing connection\n");
	Reason ->
	    Server ! next_worker,
	    io:format("Unable to accept socket because: ~p~n", [Reason]),
	    Server ! {error, Reason}
    end.

%% Will stay with the connection for the life of the connection. 
%% Has a default timeout of 10 seconds which will close the connection if there is no activity.
%% Timeout can be set to 0 on the call, this will be required to support IDLE command, for fake push notification.
command_handler(Port, Timeout) ->
    State = #imap_connection{port=Port, timeout=Timeout},
    command_handler(State).
command_handler(State) when is_record(State, imap_connection) ->
    case gen_tcp:recv(State#imap_connection.port, 0, State#imap_connection.timeout) of
	{ok, Packet} ->
	    [Input| _] = string:tokens(Packet, "\r\n"),
	    [Ident, Command |Args] = string:tokens(Input, " "),
	    %% Should really spawn a run_command so that heavy commands do not slow down the command input.
	    run_command(string:to_upper(Command), State, Ident, Args),
	    command_handler(State);
	{error, closed} ->
	    io:format("Client closed the connection. ~n", []);
	{error, timeout} ->
	    io:format("Client timed out. ~n", []);
	Reason ->
	    io:format("Unable to recv from socket because: ~p~n", [Reason])
    end;
command_handler(Port) ->
    command_handler(Port, 30000).

run_command("CAPABILITY", State, Ident, _) ->
    gen_tcp:send(State#imap_connection.port, "* CAPABILITY NOOP LOGOUT IDLE\n"),
    gen_tcp:send(State#imap_connection.port, Ident ++ " OK CAPABILITY completed\n");
run_command("IDLE", State, _, _) ->
    _ = State#imap_connection{timeout=0},
    % Should check to make sure the assignment worked.
    command_handler(State);
run_command("NOOP", State, _, _) ->
    command_handler(State);
run_command("LOGOUT", State, Ident, _) ->
    _ = State#imap_connection{state="Not Authenticated"},
    % Should check to make sure the assignment worked.
    gen_tcp:send(State#imap_connection.port,  Ident ++ " OK LOGOUT completed"),
    {ok, closed};
run_command(Command, State, Ident, _) ->
    gen_tcp:send(State#imap_connection.port, Ident ++ " BAD parse error: " ++ Command ++ " command not implemented\n"),
    command_handler(State).
