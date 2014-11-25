%% @doc Simulator.
%% The simulator that contains functions that represent the server
%% and client containers. Also contains functions to spawn a certain
%% number of servers/clients based on what the overseer module wants
%% to have spawned. Essentially, this module simulates clients pinging
%% hosts in a cluster.
%% @version 1.2
%% @TODO Add functionality to account for server load capacity.
%% Date Last Modified: 11/20/2014
-module(simulator).
-export([server/1,client/3,spawnClients/5,spawnServers/2, pickRandomServer/1]).
-import(overseer, [dictionary/1]).

%% ----------------------------------------------------------------------------
%% @doc server(State).
%% Represents a server. Maintains a server State to represent the number
%% of times a server is pinged by clients. If the server receives a 
%% request from a client, it accepts the client and sends a message back
%% to the client with the current hit count of the server and then the
%% server function is recursively called.
server(State) ->
  receive 
    {request, Return_PID} ->
      io:format("Server ~w: Client request received from ~w~n",
          [self(), Return_PID]),
      NewState = State + 1,
      Return_PID ! {hit_count, NewState},
      server(NewState)
  end.
  
%% ----------------------------------------------------------------------------  
%% @doc client(Server_Address).
%% Represents a client. The client contains a server address for the 
%% server that it is initially going to ping and a group ID (Group). The
%% client sends a message to the server with PID Server_Address with its
%% request and own process ID. The client recieves a message from the 
%% server pinged letting the client know that it is now on the server with
%% the server hit count.
%%
client(Server_Address, Group, Dict_ID) ->
  Server_Address ! {request, self()},
  io:format("For client number ~w, Group_ID: ~w~n", [self(), Group]),
  receive
    {hit_count, Number} ->
      io:format("Client ~w: Hit count was ~w, Group_ID: ~w~n~n", 
        [self(), Number, Group])
  end,
  Dict_ID ! {request, self(), Group, Server_Address}.
  %%client(Server_Address, Group, Dict_ID).

%% ----------------------------------------------------------------------------
%% @doc spawnClients(NumClients, ServerList).
%% Spawns a number of clients equal to the number of clients (NumClients)
%% that the overseer wants to spawn. The function recursively calls itself
%% to spawn a client one by one until there are no clients left to spawn.
%% For each recursive call, a random Server_PID is chosen by the function
%% pickRandomServer and a client is spawned with a random GroupID between
%% 1 and 5 that will ping the server specified by that random Server_PID
%% chosen. A timer is set to simulate the amount of time between client
%% pings on a host and then the function is recursively called. Once there
%% are no more clients to spawn, a message is printed to the console letting
%% the user know that the spawning is done.
%%
%% Note: while the clients are spawning, they are also being added to ServerDict,
%% the ordered dictionary in Overseer that is keeping track of the servers and
%% all their grouped clients.
spawnClients(NumClients, ServerList, Dict_ID, Dictionary, NumberOfGroups) when NumClients > 0 ->
  Server_PID = pickRandomServer(ServerList),
  Group_ID = random:uniform(NumberOfGroups),
  Client_PID = spawn(simulator,client,[Server_PID, Group_ID, Dict_ID]),
  TempServerDict = orddict:append(Server_PID, {Client_PID, Group_ID}, Dictionary),
  timer:sleep(random:uniform(100)),
  spawnClients(NumClients-1,ServerList, Dict_ID, TempServerDict, NumberOfGroups);
spawnClients(0, ServerList, Dict_ID, Dictionary, NumberOfGroups) ->
  io:format("Last client spawned. ~n~n"),
  Dictionary.

%% ----------------------------------------------------------------------------
%% @doc spawnServers(NumServers, ServerList).
%% Spawns a number of servers equal to the number of servers (NumServers)
%% that the overseer wants to spawn. The function prints the number of 
%% servers left to spawn, spawns a server with a certain PID, adds that
%% PID to a temporary list and prints it, and recursively calls the 
%% function with the parameters (NumServers-1, ServerList+PIDlist).
%% When there are no servers left to spawn, a message is printed to the
%% console letting the user know that all the servers have spawned and
%% ServerList, the list of the Server_PIDs from every server spawned,
%% is returned. 
spawnServers(NumServers, ServerList) when NumServers > 0 ->
  io:format("Number of servers left to spawn: ~w~n", [NumServers]),
  Server_PID = spawn(simulator, server, [0]),
  PIDlist = [Server_PID],
  io:format("Server ~w spawned.~n", [Server_PID]),
  TempServerList = ServerList ++ PIDlist,
  spawnServers(NumServers-1, TempServerList);		
spawnServers(0, ServerList) -> 
  io:format("~nAll servers spawned.~n~n"),
  ServerList.

%% ----------------------------------------------------------------------------
%% @doc pickRandomServer(ServerList). 
%% Takes in the list of all Server_PIDs spawned by the simulator and 
%% returns a random Server_PID from ServerList.
pickRandomServer(ServerList) ->
  ListIndex = random:uniform(length(ServerList)),
  Server_PID = lists:nth(ListIndex, ServerList),
  Server_PID.


