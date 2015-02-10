%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%%
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from greedy.erl.
%% @version 1.2
%% @TODO Add functionality to enter NumberOfClients, NumberOfServers,
%% and NumberOfGroups from the command line.
%% Date Last Modified: 02/10/2015

-module(overseer).
-export([main/0, store_keys/4, print_info/7, get_second_element/3, print_group_count/4,
 print_file_header/3, count_groups/4, get_num_clients/4, print_updated_clients/4]).

%% ----------------------------------------------------------------------------
%% @doc main().
%%
main() ->  

  %Initialize variables, processes.
  %NumberOfClients = 100,
  {ok, [X]} = io:fread("Enter the number of clients> ", "~d"),
  NumberOfClients = X,
  %NumberOfServers = 10,
  {ok, [Y]} = io:fread("Enter the number of servers> ", "~d"),
  NumberOfServers = Y,
  %NumberOfGroups = 7,
  {ok, [Q]} = io:fread("Enter the number of groups> ", "~d"),
  NumberOfGroups = Q,
  %ServerCapacity = 10,
  {ok, [K]} = io:fread("Enter the server capacity> ", "~d"),
  ServerCapacity = K,
  TempServerList = [],
  TempServerDict = orddict:new(),
  Dict = orddict:new(),
  Dictionary = orddict:new(),
  TempGroupList = [],
  
  %Start two processes for the input and output files of the demo.
  OneFile = file:open("before.csv", write),
  FilePID = element(2, OneFile),
  TwoFile = file:open("after.csv", write),
  FilePID2 = element(2, TwoFile),
 
  Dict_ID = spawn(overseer, dictionary, [Dict]),
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  
  %Spawn the server processes and store those PIDs in our ServerDict.
  ServerList = simulator:spawn_servers(NumberOfServers, TempServerList),
  ServerDict = store_keys(Dict, ServerList, 1, NumberOfServers),
  
  %Spawn clients, ServerDict2 now populated with {Client_PID, GroupID} tuples.
  ServerDict2 = simulator:spawn_clients(NumberOfClients, ServerList, Dict_ID, ServerDict, NumberOfGroups),
  
  %orddict:fetch(lists:nth(1, ServerList), ServerDict2),
  
  %Print file header for 'before.csv'.
  io:fwrite(FilePID, "Servers,", []),
  print_file_header(FilePID, 1, NumberOfGroups),
  
  %Print ServerDict to 'before.csv', populating GroupList at the same time.
  GroupList = print_info(ServerDict2, ServerList, FilePID, 1, NumberOfServers, NumberOfGroups, TempGroupList),

  %GroupList,
  
  %Sort the group list by use of the greedy algorithm.
  %SortedGroupList = greedy:reassign_clients(GroupList, ServerDict2, FilePID2, ServerCapacity, 2),
  SortedGroupList = greedy:do_greedy(GroupList, ServerDict2, FilePID2, ServerCapacity, 2, 1),
  
  %Print file header for 'after.csv'.
  io:fwrite(FilePID2, "Servers,", []),
  print_file_header(FilePID2, 1, NumberOfGroups),
  
  %Print updated client information to 'after.csv'.
  print_updated_clients(SortedGroupList, FilePID2, 1, NumberOfGroups),
  
  %Copy before.csv, after.csv to D3 directory from current directory.
  file:copy("before.csv", "../docs/visualization/D3/before.csv"),
  file:copy("after.csv", "../docs/visualization/D3/after.csv"),
  io:format("before.csv, after.csv moved to ../docs/visualization/D3/~n", []),
  
  %Halt the overseer.
  init:stop().

%% ----------------------------------------------------------------------------
%% @doc store_keys/4
%% This function takes care of the initial popluation of the Server_PID
%% keys into ServerDict.
%% 
store_keys(ServerDict, ServerList, ListIndex, NumberOfServers) when ListIndex =< NumberOfServers ->
  Current_PID = lists:nth(ListIndex, ServerList),
  io:format("Current PID: ~w~n", [Current_PID]),
  TempServerDict = orddict:store(Current_PID, [], ServerDict),
  TempValue = orddict:fetch(Current_PID, TempServerDict),
  io:format("Value now stored in ServerDict: ~w~n", [TempValue]),
  store_keys(TempServerDict, ServerList, ListIndex+1, NumberOfServers);
store_keys(TempServerDict, ServerList, ListIndex, NumberOfServers) ->
  io:format("Key storage complete.~n~n"),
  TempServerDict.

%% ----------------------------------------------------------------------------
%% @doc print_updated_clients/4
%% This function prints the second, updated list of server GroupLists to
%% the file 'after.csv' by printing the group count of each of the elements
%% one element of the time until the last index of SortedGroupList has been
%% printed. File_PID is the process identifier that represents 'after.csv'.
%% 
print_updated_clients(SortedGroupList, File_PID, GroupIndex, NumberOfGroups) when GroupIndex =< length(SortedGroupList) ->
  GroupCount = lists:nth(GroupIndex, SortedGroupList),
  io:fwrite(File_PID, "Server ~w,", [GroupIndex]),
  print_group_count(File_PID, GroupCount, 1, NumberOfGroups),
  print_updated_clients(SortedGroupList, File_PID, GroupIndex+1, NumberOfGroups);
print_updated_clients(SortedGroupList, File_PID, GroupIndex, NumberOfGroups) ->
  io:format("Updated client assignments printed to after.csv.~n", []).

%% ----------------------------------------------------------------------------
%% @doc print_info/7
%% This function uses several other functions present in overseer to manipulate
%% the orddict ServerDict in such a way that the information that represents
%% the number of clients in each group on each server in the simulation is 
%% printed to the file 'before.csv'. File_PID is the process identifier that
%% represents the file 'before.csv'. For every Server_PID key in ServerDict,
%% print_info does the following:
%%  -Fetches the list of keys in ServerDict (KeyList).
%%  -Picks out each Server_PID from Keylist, one by one (Server_PID).
%%  -Fetches the list of {Client_PID, GroupID} tuples for Server_PID (ClientList).
%%  -Places the GroupID of every Client_PID in Server_PID in GroupList by use of get_second_element
%%  -Counts the number of each GroupID in Server_PID and returns those values
%%   in GroupCount ([#Clients Group 1, .., #Clients Group NumberOfGroups]).
%%  -Prints each of those GroupCounts to 'before.csv' properly formatted using print_group_count.
%% Once every Server_PID is accounted for, print_info returns GroupList, which is
%% the list of every GroupCount list printed to 'before.csv'. 
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

%% ----------------------------------------------------------------------------
%% @doc get_second_element/3
%% Outputs a list of each GroupID from every {Client_PID, GroupID} tuple
%% input into the function (list of tuples is GroupList). Function does so
%% by using recursion to grab the second element of every element in 
%% GroupList, add that to TempList, which is a 'running count' of the
%% elements, then returns TempList once GroupList has been exhausted.
%% 
get_second_element(GroupList, AppendList, ElementIndex) when ElementIndex =< length(GroupList) ->
  Element = lists:nth(ElementIndex, GroupList),
  GroupNumber = [element(2, Element)],
  TempList = AppendList ++ GroupNumber,
  get_second_element(GroupList, TempList, ElementIndex+1);
get_second_element(GroupList, TempList, ElementIndex) when ElementIndex > length(GroupList) ->
  TempList.

%% ----------------------------------------------------------------------------
%% @doc count_groups/4
%% Counts the number of instances for each Group number in GroupList and
%% outputs that value in a list ActualRunningCount, which is in the form
%% [#Clients in Group 1, .., #Clients in Group NumberOfGroups]. Uses
%% the function get_num_clients to get the value of the number of instances
%% of a particular Group ID in GroupList.
%%   
count_groups(GroupList, RunningCount, NumberOfGroups, GroupIndex) when GroupIndex =< NumberOfGroups ->
  NumClientsInGroup = [get_num_clients(GroupList, GroupIndex, 0, 1)],
  io:format("NumClientsInGroup in countGroups: ~w~n", [NumClientsInGroup]),
  ActualRunningCount = RunningCount ++ NumClientsInGroup,
  count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex+1);
count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex) when GroupIndex > NumberOfGroups ->
  ActualRunningCount.
  
%% ----------------------------------------------------------------------------
%% @doc print_group_count/4
%% Prints the list GroupCount to the file represented by the process identifier
%% File_PID in a format properly recognized by D3. For the purposes of our demo,
%% the type of file being printed to is a csv file.
%% 
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

%% ----------------------------------------------------------------------------
%% @doc get_num_clients/4
%% For a particular GroupID value, this function sweeps through GroupList
%% and counts the number of times that that value appears in GroupList. This
%% is returned by the function by NewNumClients.
%%  
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
  
  %% ----------------------------------------------------------------------------
%% @doc print_file_header/3
%% Prints the properly formatted group labels to the csv file represented by the
%% process identifier File_PID.
%% 
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



    
  
 
