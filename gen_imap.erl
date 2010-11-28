-module(gen_imap).

-export([init/0, init/1, loop0/1, loop0/2, loop/1, connection_distributor/2, command_handler/2, run_command/4]).

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
	    command_handler(Port);
	Reason ->
	    Server ! next_worker,
	    io:format("Unable to accept socket because: ~p~n", [Reason])
    end.

%% Will stay with the connection for the life of the connection. 
%% Has a default timeout of 10 seconds which will close the connection if there is no activity.
%% Timeout can be set to 0 on the call, this will be required to support IDLE command, for fake push notification.
command_handler(Port) ->
    command_handler(Port, 10000);
command_handler(Port, Timeout) ->
    case gen_tcp:recv(Port, 0, Timeout) of
	{ok, Packet} ->
	    [Ident | Commands] = string:tokens(Packet, " "),
	    [Command | Args ] = Commands,
	    io:format("Got packet: ~p~n", [Packet]),
	    io:format("Port: ~p Ident: ~p Command: ~p Args: ~p~n", [Port, Ident, Command, Args]),
	    run_command(string:to_upper(Command), Port, Timeout, Ident, Args),
	    command_handler(Port);
	{error, closed} ->
	    io:format("Client closed the connection. ~n", []);
	Reason ->
	    io:format("Unable to recv from socket because: ~p~n", [Reason])
    end.

run_command("CAPABILITY", Port, Timeout, Ident, Args) ->
    gen_tcp:send(Port, "CAPABILITY NOOP IDLE\n"),
    gen_tcp:send(Port, Ident ++ " OK CAPABILITY completed\n";
run_command("IDLE", Port, Timeout, Ident, Args) ->
    command_handler(Port, 0);
run_command("NOOP", Port, Timeout, Ident, Args) ->
    command_handler(Port, Timeout);
run_command("LOGOUT", Port, Timeout, Ident, Args) ->
    {ok, closed};
run_command(Command, Port, Timeout, Ident, Args) ->
    gen_tcp:send(Port, "BAD parse error: " ++ Command ++ "command not implemented\n").
