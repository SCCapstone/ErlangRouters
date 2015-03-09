%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%%
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from greedy.erl.
%% @version 1.2.1
%% @TODO Add functionality to enter NumberOfClients, NumberOfServers,
%% and NumberOfGroups from the command line.
%% Date Last Modified: 02/10/2015

-module(overseer).
-export([main/0, print_info/5, get_second_element/3, print_group_count/4,
 print_file_header/3, count_groups/4, get_num_clients/4, print_updated_clients/4,
 get_matched_clients/1, master_server/0, handle_client_movement/3, 
 insert_new_client/4]).

%% ----------------------------------------------------------------------------
%% @doc main().
%%
main() ->  

    %Initialize variables, processes.
    NumberOfClients = 25,
    %{ok, [X]} = io:fread("Enter the number of clients> ", "~d"),
    %NumberOfClients = X,
    NumberOfServers = 5,
    %{ok, [Y]} = io:fread("Enter the number of servers> ", "~d"),
    %NumberOfServers = Y,
    NumberOfGroups = 5,
    %{ok, [Q]} = io:fread("Enter the number of groups> ", "~d"),
    %NumberOfGroups = Q,
    ServerCapacity = 10,
    %{ok, [K]} = io:fread("Enter the server capacity> ", "~d"),
    %ServerCapacity = K,
  
    ets:new(server_list, [ordered_set, named_table, public]),
    ets:new(dictionary, [ordered_set, {keypos,1}, named_table, public]),
    
    simulator:spawn_servers(NumberOfServers, ServerCapacity),
    simulator:spawn_clients(NumberOfClients, NumberOfGroups),
  
    %Start two processes for the input and output files of the demo.
    OneFile = file:open("before.csv", [write]),
    FilePID = element(2, OneFile),
    TwoFile = file:open("after.csv", [write]),
    FilePID2 = element(2, TwoFile),
  
    %Print file header for 'before.csv'.
    io:fwrite(FilePID, "Servers,", []),
    print_file_header(FilePID, 1, NumberOfGroups),
  
    %Print ServerDict to 'before.csv', populating GroupList at the same time.
    GroupList = print_info(FilePID, 1, NumberOfServers, NumberOfGroups, []),
  
    io:format("Dictionary to list before: ~w~n", [ets:tab2list(dictionary)]),
    %Sort the group list by use of the greedy algorithm.
    SortedGroupList = greedy:do_greedy(GroupList, FilePID2, ServerCapacity, 1),
    io:format("Dictionary to list after: ~w~n", [ets:tab2list(dictionary)]),
    %Print file header for 'after.csv'.
    io:fwrite(FilePID2, "Servers,", []),
    print_file_header(FilePID2, 1, NumberOfGroups),
  
    %Print updated client information to 'after.csv'.
    print_updated_clients(SortedGroupList, FilePID2, 1, NumberOfGroups),
    
    file:delete("../docs/visualization/D3/before.csv"),
    file:delete("../docs/visualization/D3/after.csv"),
    
    %Copy before.csv, after.csv to D3 directory from current directory.
    file:copy("before.csv", "../docs/visualization/D3/before.csv"),
    file:copy("after.csv", "../docs/visualization/D3/after.csv"),
    
    %Delete csv files in current directory.
    file:delete("before.csv"),
    file:delete("after.csv"),
  
    %Halt the overseer.
    init:stop().

master_server() ->
    receive
        {print_group_data, GroupList} ->
            master_server()
    end.
%% ----------------------------------------------------------------------------
%% @doc print_updated_clients/4
%% This function prints the second, updated list of server GroupLists to
%% the file 'after.csv' by printing the group count of each of the elements
%% one element of the time until the last index of SortedGroupList has been
%% printed. File_PID is the process identifier that represents 'after.csv'.
%% 
print_updated_clients(SortedGroupList, File_PID, GroupIndex, NumberOfGroups) 
    when GroupIndex =< length(SortedGroupList) ->
    
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
print_info(File_PID, ServerCount, NumberOfServers, NumberOfGroups, FullGroupList) 
    when ServerCount =< NumberOfServers ->

    ServerList = ets:tab2list(server_list),
    Server_PID = element(1,lists:nth(ServerCount, ServerList)),
    ClientList = overseer:get_matched_clients(Server_PID),
    GroupList = get_second_element(ClientList, [], 1),
    GroupCount = count_groups(GroupList, [], NumberOfGroups, 1),
    io:fwrite(File_PID, "Server ~w,", [ServerCount]),
    print_group_count(File_PID, GroupCount, 1, NumberOfGroups),
    TempGroupList = FullGroupList ++ [GroupCount],
    print_info(File_PID, ServerCount+1, NumberOfServers, NumberOfGroups, TempGroupList);

print_info(File_PID, ServerCount, NumberOfServers, NumberOfGroups, GroupList) 
    when ServerCount > NumberOfServers ->
    
    GroupList.

%% ----------------------------------------------------------------------------
%% @doc get_matched_clients/1
%% Returns the a list of lists ClientMatches, [[Client_PID, Group_ID]], in which each 
%% element of the ClientMatches is a [Client_PID, Group_ID] pair. The list of pairs
%% represents every dictionary entry that has a match of its '$2' tuple position with
%% Server_PID.
%%
%% Input: Server_PID- Server_PID to be matched.
%% Output: ClientMatches- List of lists, where each element is a match in the dictionary
%% to the particular Server_PID input.
get_matched_clients(Server_PID) -> 
    ClientMatches = ets:select(dictionary, [{{'$1',Server_PID,'$3'},[],['$$']}]),
    ClientMatches.

%% ----------------------------------------------------------------------------
%% @doc handle_client_movement/3
%% Updates dictionary by moving all clients from RemovalServer_PID in group Group_ID
%% to AdditionServer_PID.
%%
%% Input: RemovalServer_PID- server clients are being removed from.
%%        AdditionServer_PID- server clients are being added to.
%%        Group_ID- group ID number of clients that are being moved.
%% Output: None.
handle_client_movement(RemovalServer_PID, AdditionServer_PID, Group_ID) ->
    ClientsMoved = ets:select(dictionary, [{{'$1', RemovalServer_PID, Group_ID}, 
        [], ['$$']}]),
    io:format("ZORK: ~w~n", [ClientsMoved]),
    insert_new_client(ClientsMoved, AdditionServer_PID, Group_ID, 1).

%% ----------------------------------------------------------------------------
%% @doc insert_new_client/4
%% Takes in a list of clients that need to be moved to server AdditionServer_PID.
%% This list, ClientList, is iterated through ClientIndex number of times. For
%% each iteration, a {Client_PID, AdditionServer_PID, Group_ID} tuple is inserted
%% into the dictionary. Because the dictionary is an ordered_set, the previous
%% entry for Client_PID is replaced with the new entry.
%%
%% Input: ClientList- List of clients that need to be moved.
%%        AdditionServer_PID- server clients are being added to.
%%        Group_ID- group ID number of clients that are being moved.
%%        ClientIndex- iterator for the ClientList.
%% Output: None.
insert_new_client(ClientList, AdditionServer_PID, Group_ID, ClientIndex) ->
    case ClientIndex =< length(ClientList) of
        true ->
            Client_PID = lists:nth(1, lists:nth(ClientIndex, ClientList)),
            ets:insert(dictionary, {Client_PID, AdditionServer_PID, Group_ID}),
            insert_new_client(ClientList, AdditionServer_PID, Group_ID, 
                ClientIndex+1);
        false ->
            ok
    end.
    
%% ----------------------------------------------------------------------------
%% @doc get_second_element/3
%% Outputs a list of each GroupID from every [Client_PID, GroupID] list
%% input into the function (list of lists is GroupList). Function does so
%% by using recursion to grab the second element of every element in 
%% GroupList, add that to TempList, which is a 'running count' of the
%% elements, then returns TempList once GroupList has been exhausted.
get_second_element(GroupList, AppendList, ElementIndex) 
    when ElementIndex =< length(GroupList) ->
    
    Element = lists:nth(ElementIndex, GroupList),
    GroupNumber = [lists:nth(2, Element)],
    TempList = AppendList ++ GroupNumber,
    get_second_element(GroupList, TempList, ElementIndex+1);

get_second_element(GroupList, TempList, ElementIndex) 
    when ElementIndex > length(GroupList) ->
    
    TempList.

%% ----------------------------------------------------------------------------
%% @doc count_groups/4
%% Counts the number of instances for each Group number in GroupList and
%% outputs that value in a list ActualRunningCount, which is in the form
%% [#Clients in Group 1, .., #Clients in Group NumberOfGroups]. Uses
%% the function get_num_clients to get the value of the number of instances
%% of a particular Group ID in GroupList.
count_groups(GroupList, RunningCount, NumberOfGroups, GroupIndex) 
    when GroupIndex =< NumberOfGroups ->
  
    NumClientsInGroup = [get_num_clients(GroupList, GroupIndex, 0, 1)],
    ActualRunningCount = RunningCount ++ NumClientsInGroup,
    count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex+1);
    
count_groups(GroupList, ActualRunningCount, NumberOfGroups, GroupIndex) 
    when GroupIndex > NumberOfGroups ->
    
    ActualRunningCount.
  
%% ----------------------------------------------------------------------------
%% @doc print_group_count/4
%% Prints the list GroupCount to the file represented by the process identifier
%% File_PID in a format properly recognized by D3. For the purposes of our demo,
%% the type of file being printed to is a csv file.
print_group_count(File_PID, GroupCount, CountIndex, NumberOfGroups) 
    when CountIndex =< NumberOfGroups ->
    
    GroupElement = lists:nth(CountIndex, GroupCount),
    
    case CountIndex =< NumberOfGroups-1 of
        true->
            io:fwrite(File_PID, "~w,", [GroupElement]);
        false ->
            io:fwrite(File_PID, "~w", [GroupElement])
    end,
    
    print_group_count(File_PID, GroupCount, CountIndex+1, NumberOfGroups);

print_group_count(File_PID, GroupCount, CountIndex, NumberOfGroups) 
    when CountIndex > NumberOfGroups ->
    
    io:fwrite(File_PID, "~n", []).

%% ----------------------------------------------------------------------------
%% @doc get_num_clients/4
%% For a particular GroupID value, this function sweeps through GroupList
%% and counts the number of times that that value appears in GroupList. This
%% is returned by the function by NewNumClients.
get_num_clients(GroupList, GroupIndex, NumClients, GroupListIndex) 
    when GroupListIndex =< length(GroupList) ->
  
    GroupNum = lists:nth(GroupListIndex, GroupList),
    
    case GroupNum == GroupIndex of
        true ->
            NewNumClients = NumClients + 1;
        false ->
            NewNumClients = NumClients
    end,
    
    get_num_clients(GroupList, GroupIndex, NewNumClients, GroupListIndex+1);

get_num_clients(GroupList, GroupIndex, NewNumClients, GroupListIndex) 
    when GroupListIndex > length(GroupList) -> 
    NewNumClients.
  
  %% ----------------------------------------------------------------------------
%% @doc print_file_header/3
%% Prints the properly formatted group labels to the csv file represented by the
%% process identifier File_PID.
print_file_header(File_PID, GroupCount, NumberOfGroups) 
    when GroupCount =< NumberOfGroups ->
  
    case GroupCount =< NumberOfGroups-1 of
        true ->
            io:fwrite(File_PID, "Group ~w,", [GroupCount]);
        false ->
            io:fwrite(File_PID, "Group ~w", [GroupCount])
    end,
    
    print_file_header(File_PID, GroupCount+1, NumberOfGroups);

print_file_header(File_PID, GroupCount, NumberOfGroups) 
    when GroupCount > NumberOfGroups ->
    io:fwrite(File_PID, "~n", []).
    

    
  
 
