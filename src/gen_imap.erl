-module(gen_imap).
-author("William King <quentusrex@gmail.com>").
-export([init/0, init/1, loop/1]).

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
	    spawn_link(gen_imap_connection, connection_distributor, [self(), Socket]);
	{ok, _} ->
	    ok;
	Reason ->
	    io:format("Error: ~p~n", [Reason])
    end,
    loop(Socket).
