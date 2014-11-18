%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from TBA.
%% @version 1.1
%% @TODO Add functionality to enter NumberOfClients, NumberOfServers,
%% and NumberOfGroups from the command line.
%% Date Last Modified: 11/18/2014

-module(overseer).
-export([main/0]).
-import(simulator, [spawnServers/2, spawnClients/2]).

%% ----------------------------------------------------------------------------
%% @doc main().
%% Establishes a NumberOfClients, NumberOfServers, and an empty initial
%% TempServerList for the simulation. First, the ServerList is populated
%% and the servers are spawned by simulator:spawnServers(NumberOfServers,
%% TempServerList). Then, the clients are spawned by simulator:
%% spawnClients(NumberOfClients, ServerList).
main() ->  
  NumberOfClients = 100,
  NumberOfServers = 10,
  TempServerList = [],
  
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  
  ServerList = spawnServers(NumberOfServers, TempServerList),
  spawnClients(NumberOfClients, ServerList).
 
