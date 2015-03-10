%% @doc Simulator.
%% The simulator that contains functions that represent the server
%% and client containers. Also contains functions to spawn a certain
%% number of servers/clients based on what the overseer module wants
%% to have spawned. Essentially, this module simulates clients pinging
%% hosts in a cluster.
%% @version 1.2.1
%% @TODO Add functionality to account for server load capacity.
%% Date Last Modified: 12/05/14
-module(simulator).
-export([server/2,client/2,spawn_clients/2,spawn_servers/2, pick_random_server/1]).

%% ----------------------------------------------------------------------------
%% @doc server(State).
%% Represents a server. Maintains a server State to represent the number
%% of times a server is pinged by clients. If the server receives a 
%% request from a client, it accepts the client and sends a message back
%% to the client with the current hit count of the server and then the
%% server function is recursively called.
server(State, ServerCapacity) ->
    receive
		{request, Return_PID, Group_ID} ->
			case State < ServerCapacity of
				true ->
					io:format("Server ~w: Client request received from ~w~n",
						[self(), Return_PID]),
					NewState = State + 1,
					ets:insert(dictionary, {Return_PID, self(), Group_ID}),
					Return_PID ! {hit_count, NewState},
					server(NewState, ServerCapacity);
				false ->
					io:format("Server ~w full.~n", [self()]),
					Return_PID ! {server_full},
					server(State, ServerCapacity)
			end;
		{capacity_request, Return_PID} ->
			Return_PID ! {server_capacity, State, ServerCapacity},
			server(State, ServerCapacity)
	end.
  
%% ----------------------------------------------------------------------------  
%% @doc client(Server_Address).
%% Represents a client. The client contains a server address for the 
%% server that it is initially going to ping and a group ID (Group). The
%% client sends a message to the server with PID Server_Address with its
%% request and own process ID. The client recieves a message from the 
%% server pinged letting the client know that it is now on the server with
%% the server hit count.
client(Server_Address, Group) ->
	Server_Address ! {request, self(), Group},
	io:format("For client number ~w, Group_ID: ~w~n", [self(), Group]),
	receive
		{hit_count, Number} ->
			io:format("Client ~w: Hit count was ~w, Group_ID: ~w~n~n", 
				[self(), Number, Group]);
		{server_full} ->
			Master_PID = whereis(master_server),
			Master_PID ! {nonfull_server_request, self()},
			receive
				{new_server_pid, NewServer_PID} ->
					client(NewServer_PID, Group)
			end
		
	end.

%% ----------------------------------------------------------------------------
%% @doc spawn_clients/2
%% Spawns a number of clients equal to NumberOfClients. The function first
%% selects a random Server_PID from server_list. Then, a Group_ID is randomly
%% chosen between groups 1-NumberOfGroups. A {Client_PID, Server_PID, Group_ID}
%% tuple is then inserted into the dictionary. A timer is run randomly between
%% 1-100 ms to simulate the time in between clients joining a cluster. Once the
%% timer is up, the next client is spawned.
%%
%% Input: NumberOfClients- Number of Clients to be spawned by the function
%%        NumberOfGroups- Number of different groups clients can be a part of
%% Output: None.
spawn_clients(NumberOfClients, NumberOfGroups) when NumberOfClients > 0 ->
    ServerList = ets:tab2list(server_list),
    Server_PID = pick_random_server(ServerList),
    Group_ID = random:uniform(NumberOfGroups),
    spawn(simulator,client,[Server_PID, Group_ID]),
    timer:sleep(random:uniform(100)),
    spawn_clients(NumberOfClients-1, NumberOfGroups);
spawn_clients(0, NumberOfGroups) ->
    io:format("Last client spawned. ~n~n").
  
%% ----------------------------------------------------------------------------
%% @doc spawn_servers(NumberOfServers, ServerCapacity).
%% Spawns a number of server processes equal to NumberOfServers. A server is
%% spawned, with a Server_PID of the process recorded. That Server_PID is then
%% inserted into the server_list. Servers are spawned until the NumberOfServers
%% left to spawn is 0.
%%
%% Input: NumberOfServers- the number of servers to be spawned
%% Ouput: None 
spawn_servers(NumberOfServers, ServerCapacity) when NumberOfServers > 0 ->
    io:format("Number of servers left to spawn: ~w~n", [NumberOfServers]),
    Server_PID = spawn(simulator, server, [0, ServerCapacity]),
    ets:insert(server_list, {Server_PID}),
    io:format("Server ~w spawned.~n", [Server_PID]),
    spawn_servers(NumberOfServers-1, ServerCapacity);   
spawn_servers(0, ServerCapacity) -> 
    io:format("~nAll servers spawned.~n~n").

%% ----------------------------------------------------------------------------
%% @doc pick_random_server(ServerList). 
%% Takes in the list of all Server_PIDs spawned by the simulator and 
%% returns a random Server_PID from ServerList.
pick_random_server(ServerList) ->
    ListIndex = random:uniform(length(ServerList)),
    ServerTuple = lists:nth(ListIndex, ServerList),
    Server_PID = element(1, ServerTuple),
    Server_PID.


