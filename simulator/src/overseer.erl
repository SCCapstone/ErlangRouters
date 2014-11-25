%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from TBA.
%% @version 1.1
%% @TODO Add functionality to enter NumberOfClients, NumberOfServers,
%% and NumberOfGroups from the command line.
%% Date Last Modified: 11/20/2014

-module(overseer).
-export([main/0, storeKeys/4, printInfo/6, getSecondElement/3, printGroupCount/4, printFileHeader/3, countGroups/4, getNumClients/4, dictionary/1]).
-import(simulator, [spawnServers/2, spawnClients/5, client/3]).

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
  NumberOfGroups = 7,
  TempServerList = [],
  TempServerDict = orddict:new(),
  Dict = orddict:new(),
  Dictionary = orddict:new(),
  
  OneFile = file:open("before.csv", write),
  FilePID = element(2, OneFile),
  %register(File1, FilePID),
  
  Dict_ID = spawn(overseer, dictionary, [Dict]),
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  
  ServerList = spawnServers(NumberOfServers, TempServerList),
  ServerDict = storeKeys(Dict, ServerList, 1, NumberOfServers),
  
  ServerDict2 = spawnClients(NumberOfClients, ServerList, Dict_ID, ServerDict, NumberOfGroups),
  
  orddict:fetch(lists:nth(1, ServerList), ServerDict2),
  
  io:fwrite(FilePID, "Servers ", []),
  printFileHeader(FilePID, 1, NumberOfGroups),
  
  printInfo(ServerDict2, ServerList, FilePID, 1, NumberOfServers, NumberOfGroups).

%% ----------------------------------------------------------------------------
%% @doc storeKeys().
%% 
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

printInfo(ServerDict, ServerList, File_PID, ServerCount, NumberOfServers, NumberOfGroups) when ServerCount =< NumberOfServers ->
  KeyList = orddict:fetch_keys(ServerDict),
  Server_PID = lists:nth(ServerCount, ServerList),
  io:format("Server_PID in printInfo: ~w~n", [Server_PID]),
  ClientList = orddict:fetch(Server_PID, ServerDict),
  io:format("ClientList in printInfo: ~w~n", [ClientList]),
	GroupList = getSecondElement(ClientList, [], 1),
	io:format("GroupList in printInfo: ~w~n", [GroupList]),
	GroupCount = countGroups(GroupList, [], NumberOfGroups, 1),
	io:format("GroupCount in printInfo: ~w~n", [GroupCount]),
	io:fwrite(File_PID, "Server~w ", [ServerCount]),
	printGroupCount(File_PID, GroupCount, 1, NumberOfGroups),
	printInfo(ServerDict, ServerList, File_PID, ServerCount+1, NumberOfServers, NumberOfGroups);
printInfo(ServerDict, ServerList, File_PID, ServerCount, NumberOfServers, NumberOfGroups) when ServerCount > NumberOfServers ->
	io:format("Group info printed to csv file. ~n", []).

printGroupCount(File_PID, GroupCount, CountIndex, NumberOfGroups) when CountIndex =< NumberOfGroups ->
	GroupElement = lists:nth(CountIndex, GroupCount),
	io:fwrite(File_PID, "~w ", [GroupElement]),
	printGroupCount(File_PID, GroupCount, CountIndex+1, NumberOfGroups);
printGroupCount(File_PID, GroupCount, CountIndex, NumberOfGroups) when CountIndex > NumberOfGroups ->
  io:fwrite(File_PID, "~n", []),
  io:format("Correct formatted GroupCount added to file~n", []).

printFileHeader(File_PID, GroupCount, NumberOfGroups) when GroupCount =< NumberOfGroups ->
	io:fwrite(File_PID, "Group~w ", [GroupCount]),
	printFileHeader(File_PID, GroupCount+1, NumberOfGroups);
printFileHeader(File_PID, GroupCount, NumberOfGroups) when GroupCount > NumberOfGroups ->
	io:fwrite(File_PID, "~n", []),
	io:format("Header printed to file.", []).
	
getSecondElement(GroupList, AppendList, ElementIndex) when ElementIndex =< length(GroupList) ->
  Element = lists:nth(ElementIndex, GroupList),
  GroupNumber = [element(2, Element)],
  TempList = AppendList ++ GroupNumber,
  getSecondElement(GroupList, TempList, ElementIndex+1);
getSecondElement(GroupList, TempList, ElementIndex) when ElementIndex > length(GroupList) ->
  TempList.
    
countGroups(GroupList, RunningCount, NumberOfGroups, GroupIndex) when GroupIndex =< NumberOfGroups ->
	NumClientsInGroup = [getNumClients(GroupList, GroupIndex, 0, 1)],
	io:format("NumClientsInGroup in countGroups: ~w~n", [NumClientsInGroup]),
	ActualRunningCount = RunningCount ++ NumClientsInGroup,
	countGroups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex+1);
countGroups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex) when GroupIndex > NumberOfGroups ->
	ActualRunningCount.

getNumClients(GroupList, GroupIndex, NumClients, GroupListIndex) when GroupListIndex =< length(GroupList) ->
	GroupNum = lists:nth(GroupListIndex, GroupList),
	io:format("GroupNum in getNumClients: ~w~n", [GroupNum]),
	if
		GroupNum == GroupIndex ->
			NewNumClients = NumClients + 1;
		GroupNum =/= GroupIndex ->
		  NewNumClients = NumClients
	end,
	getNumClients(GroupList, GroupIndex, NewNumClients, GroupListIndex+1);
getNumClients(GroupList, GroupIndex, NewNumClients, GroupListIndex) when GroupListIndex > length(GroupList) -> 
	NewNumClients.
	
dictionary(Dict) ->
	receive
	  {request, Client_PID, Group, Server_Address} ->
			io:format("Following Client/Group pair added to the dictionary: ~w, ~w for server: ~w~n~n",
			  [Client_PID, Group, Server_Address]),
			orddict:append(Server_Address, {Client_PID, Group}, Dict),
			dictionary(Dict)
	end.
		
	
 
