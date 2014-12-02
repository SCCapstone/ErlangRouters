%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from TBA.
%% @version 1.1
%% @TODO Add functionality to enter NumberOfClients, NumberOfServers,
%% and NumberOfGroups from the command line.
%% Date Last Modified: 12/02/2014

-module(overseer).
-export([main/0, store_keys/4, print_info/7, get_second_element/3, print_group_count/4,
 print_file_header/3, count_groups/4, get_num_clients/4, dictionary/1, print_updated_clients/4]).
-import(simulator, [spawn_servers/2, spawn_clients/5, client/3]).

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
  ServerCapacity = 10,
  TempServerList = [],
  TempServerDict = orddict:new(),
  Dict = orddict:new(),
  Dictionary = orddict:new(),
  TempGroupList = [],

  OneFile = file:open("before.csv", write),
  FilePID = element(2, OneFile),
  TwoFile = file:open("after.csv", write),
  FilePID2 = element(2, TwoFile),
 
  Dict_ID = spawn(overseer, dictionary, [Dict]),
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  
  ServerList = spawn_servers(NumberOfServers, TempServerList),
  ServerDict = store_keys(Dict, ServerList, 1, NumberOfServers),
  
  ServerDict2 = spawn_clients(NumberOfClients, ServerList, Dict_ID, ServerDict, NumberOfGroups),
  
  orddict:fetch(lists:nth(1, ServerList), ServerDict2),
  
  io:fwrite(FilePID, "Servers,", []),
  print_file_header(FilePID, 1, NumberOfGroups),
  
  GroupList = print_info(ServerDict2, ServerList, FilePID, 1, NumberOfServers, NumberOfGroups, TempGroupList),
  
  GroupList,
  
  SortedGroupList = greedy:reassign_clients(GroupList, ServerDict2, FilePID2, ServerCapacity, 2),
  
  io:fwrite(FilePID2, "Servers,", []),
  print_file_header(FilePID2, 1, NumberOfGroups),
  
  print_updated_clients(SortedGroupList, FilePID2, 1, NumberOfGroups).

%% ----------------------------------------------------------------------------
%% @doc storeKeys().
%% 
store_keys(ServerDict, ServerList, ListIndex, NumberOfServers) when ListIndex =< NumberOfServers ->
  Current_PID = lists:nth(ListIndex, ServerList),
  io:format("Current PID: ~w~n", [Current_PID]),
  TempServerDict = orddict:store(Current_PID, [], ServerDict),
  TempValue = orddict:fetch(Current_PID, TempServerDict),
  io:format("Value now stored in ServerDict: ~w~n", [TempValue]),
  store_keys(TempServerDict, ServerList, ListIndex+1, NumberOfServers);
store_keys(TempServerDict, ServerList, 11, NumberOfServers) ->
  io:format("Key storage complete.~n~n"),
  TempServerDict.

print_updated_clients(SortedGroupList, File_PID, GroupIndex, NumberOfGroups) when GroupIndex =< length(SortedGroupList) ->
	GroupCount = lists:nth(GroupIndex, SortedGroupList),
	io:fwrite(File_PID, "Server ~w,", [GroupIndex]),
	print_group_count(File_PID, GroupCount, 1, NumberOfGroups),
	print_updated_clients(SortedGroupList, File_PID, GroupIndex+1, NumberOfGroups);
print_updated_clients(SortedGroupList, File_PID, GroupIndex, NumberOfGroups) ->
	io:format("Updated client assignments printed to after.csv.~n", []).
	
	
	
print_info(ServerDict, ServerList, File_PID, ServerCount, NumberOfServers, NumberOfGroups, FullGroupList) when ServerCount =< NumberOfServers ->
  KeyList = orddict:fetch_keys(ServerDict),
  Server_PID = lists:nth(ServerCount, ServerList),
  io:format("Server_PID in printInfo: ~w~n", [Server_PID]),
  ClientList = orddict:fetch(Server_PID, ServerDict),
  io:format("ClientList in printInfo: ~w~n", [ClientList]),
  GroupList = get_second_element(ClientList, [], 1),
  io:format("GroupList in printInfo: ~w~n", [GroupList]),
  GroupCount = count_groups(GroupList, [], NumberOfGroups, 1),
  io:format("GroupCount in printInfo: ~w~n", [GroupCount]),
  io:fwrite(File_PID, "Server ~w,", [ServerCount]),
  print_group_count(File_PID, GroupCount, 1, NumberOfGroups),
  TempGroupList = FullGroupList ++ [GroupCount],
  print_info(ServerDict, ServerList, File_PID, ServerCount+1, NumberOfServers, NumberOfGroups, TempGroupList);
print_info(ServerDict, ServerList, File_PID, ServerCount, NumberOfServers, NumberOfGroups, GroupList) when ServerCount > NumberOfServers ->
  io:format("Group info printed to csv file. ~n", []),
  GroupList.

print_group_count(File_PID, GroupCount, CountIndex, NumberOfGroups) when CountIndex =< NumberOfGroups ->
  GroupElement = lists:nth(CountIndex, GroupCount),
  if
    CountIndex =< NumberOfGroups-1 ->
      io:fwrite(File_PID, "~w,", [GroupElement]);
    CountIndex == NumberOfGroups ->
      io:fwrite(File_PID, "~w", [GroupElement])
  end,
  print_group_count(File_PID, GroupCount, CountIndex+1, NumberOfGroups);
print_group_count(File_PID, GroupCount, CountIndex, NumberOfGroups) when CountIndex > NumberOfGroups ->
  io:fwrite(File_PID, "~n", []),
  io:format("Correct formatted GroupCount added to file~n", []).

print_file_header(File_PID, GroupCount, NumberOfGroups) when GroupCount =< NumberOfGroups ->
  if
    GroupCount =< NumberOfGroups-1 ->
      io:fwrite(File_PID, "Group ~w,", [GroupCount]);
    GroupCount == NumberOfGroups ->
      io:fwrite(File_PID, "Group ~w", [GroupCount])
  end,
  print_file_header(File_PID, GroupCount+1, NumberOfGroups);
print_file_header(File_PID, GroupCount, NumberOfGroups) when GroupCount > NumberOfGroups ->
  io:fwrite(File_PID, "~n", []),
  io:format("Header printed to file.~n", []).
  
get_second_element(GroupList, AppendList, ElementIndex) when ElementIndex =< length(GroupList) ->
  Element = lists:nth(ElementIndex, GroupList),
  GroupNumber = [element(2, Element)],
  TempList = AppendList ++ GroupNumber,
  get_second_element(GroupList, TempList, ElementIndex+1);
get_second_element(GroupList, TempList, ElementIndex) when ElementIndex > length(GroupList) ->
  TempList.
    
count_groups(GroupList, RunningCount, NumberOfGroups, GroupIndex) when GroupIndex =< NumberOfGroups ->
  NumClientsInGroup = [get_num_clients(GroupList, GroupIndex, 0, 1)],
  io:format("NumClientsInGroup in countGroups: ~w~n", [NumClientsInGroup]),
  ActualRunningCount = RunningCount ++ NumClientsInGroup,
  count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex+1);
count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex) when GroupIndex > NumberOfGroups ->
  ActualRunningCount.

get_num_clients(GroupList, GroupIndex, NumClients, GroupListIndex) when GroupListIndex =< length(GroupList) ->
  GroupNum = lists:nth(GroupListIndex, GroupList),
  io:format("GroupNum in getNumClients: ~w~n", [GroupNum]),
  if
    GroupNum == GroupIndex ->
      NewNumClients = NumClients + 1;
    GroupNum =/= GroupIndex ->
      NewNumClients = NumClients
  end,
  get_num_clients(GroupList, GroupIndex, NewNumClients, GroupListIndex+1);
get_num_clients(GroupList, GroupIndex, NewNumClients, GroupListIndex) when GroupListIndex > length(GroupList) -> 
  NewNumClients.
  
dictionary(Dict) ->
  receive
    {request, Client_PID, Group, Server_Address} ->
      io:format("Following Client/Group pair added to the dictionary: ~w, ~w for server: ~w~n~n",
        [Client_PID, Group, Server_Address]),
      TempDict = orddict:append(Server_Address, {Client_PID, Group}, Dict),
      dictionary(TempDict)
  end.
    
  
 
