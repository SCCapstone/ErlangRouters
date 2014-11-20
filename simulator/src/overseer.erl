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
-export([main/0, storeKeys/4]).
-import(simulator, [spawnServers/2, spawnClients/3]).

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
  TempServerDict = orddict:new(),
  
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  
  ServerList = spawnServers(NumberOfServers, TempServerList),
  ServerDict = storeKeys(TempServerDict, ServerList, 1, NumberOfServers),
  
  ServerDict2 = spawnClients(NumberOfClients, ServerList, ServerDict).
  
storeKeys(ServerDict, ServerList, ListIndex, NumberOfServers) when ListIndex =< NumberOfServers ->
		Current_PID = lists:nth(ListIndex, ServerList),
		io:format("Current PID: ~w~n", [Current_PID]),
		TempServerDict = orddict:store(Current_PID, [], ServerDict),
		TempValue = orddict:fetch(Current_PID, TempServerDict),
		io:format("Value now stored in ServerDict: ~w~n", [TempValue]),
		storeKeys(TempServerDict, ServerList, ListIndex+1, NumberOfServers);
storeKeys(TempServerDict, ServerList, 11, NumberOfServers) ->
	io:format("Key storage complete.~n~n"),
	TempServerDict.
		
	
 
