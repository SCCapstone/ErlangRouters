%% @doc Overseer.
%% The overseer module manages the simulation of the servers and clients
%% and manages the algorithm for load balancing the groups on the servers.
%%
%% The functions needed for the simulation are imported from simulator.erl.
%% The functions needed for the algorithm are imported from greedy.erl/
%%     firstworst.erl.
%% @version 1.3
%% @TODO Add health_check server functionality.

-module(overseer).
-export([main/0, get_second_element/3, print_group_count/4,
 print_file_header/3, count_groups/4, get_num_clients/4, 
 get_matched_clients/1, master_server/1,
 handle_client_movement/4, insert_new_client/4, get_group_list/4, 
 load_balancer/4, print_group_list/4, spawn_files/0]).
 
%% ----------------------------------------------------------------------------
%% @doc main/0
%% Takes in user input for the number of clients, number of servers, number of
%% groups, server capacity, and which algorithm is to be used by the simulator.
%% Spawns and registers the master_server. Creates an ets server_list to hold
%% the PIDs of the servers spawned by the simulation and an ets dictionary to
%% hold the {Client_PID, Server_PID, Group_ID} tuples to keep track of the 
%% clients. Spawns the output files. Spawns servers and clients. Prints
%% initial GroupList to output. Calls load_balancer/4, which runs the
%% algorithm. Deletes the previous version of the output in the D3 directory,
%% copies the new output in the current directory to the D3 directory, and
%% deletes the output in the current directory.
main() ->  

    {ok, [X]} = io:fread("Enter the number of clients> ", "~d"),
    NumberOfClients = X,
    {ok, [Y]} = io:fread("Enter the number of servers> ", "~d"),
    NumberOfServers = Y,
    {ok, [Q]} = io:fread("Enter the number of groups> ", "~d"),
    NumberOfGroups = Q,
    {ok, [K]} = io:fread("Enter the server capacity> ", "~d"),
    ServerCapacity = K,
    {ok, [Alg]} = io:fread("Enter the algorithm you wish to use for the 
        simulation (greedy or firstworst)> ", "~s"),
    Algorithm = Alg,
    
    register(master_server, spawn(overseer, master_server, [NumberOfGroups])),
    
    ets:new(server_list, [ordered_set, named_table, public]),
    ets:new(dictionary, [ordered_set, {keypos,1}, named_table, public]),
    
    spawn_files(),
    
    simulator:spawn_servers(NumberOfServers, ServerCapacity),
    simulator:spawn_clients(NumberOfClients, NumberOfGroups),
    
    io:fwrite(whereis(before), "Servers,", []),
    print_file_header(whereis(before), 1, NumberOfGroups),
    AltGroupList = get_group_list(NumberOfGroups, NumberOfServers, [], 1),
    print_group_list(whereis(before), AltGroupList, 1, NumberOfGroups),
    
    load_balancer(NumberOfGroups, NumberOfServers, ServerCapacity,
        Algorithm),
    
    timer:sleep(5000),
    
    file:delete("../docs/visualization/D3/before.csv"),
    file:delete("../docs/visualization/D3/after.csv"),
    
    file:copy("before.csv", "../docs/visualization/D3/before.csv"),
    file:copy("after.csv", "../docs/visualization/D3/after.csv"),
    
    file:delete("before.csv"),
    file:delete("after.csv"),
    
    init:stop().

%% ----------------------------------------------------------------------------
%% @doc master_server/1
%% Spawned at the beginning of the simulation. Handles the message passing
%% in between the algorithm and client/server layers.
%%
%% Messages received:
%%     print_group_list- receives a GroupList and prints it to the file
%%         specified by process File_PID.
%%     nonfull_server_request- a request made by a client when the server
%%         that it attempts to join is full. The master_server selects a 
%%         a new server for the client and sends the PID of that server
%%         back to the client.
%%
master_server(NumberOfGroups) ->
    receive
        {print_group_list, GroupList} ->
            File_PID = whereis(notbefore),
            io:fwrite(File_PID, "Servers,", []),
            print_file_header(File_PID, 1, NumberOfGroups),
            print_group_list(File_PID, GroupList, 1, NumberOfGroups),
            master_server(NumberOfGroups);
            
        {nonfull_server_request, Return_PID} ->
            ServerList = ets:tab2list(server_list),
            NewServer_PID = simulator:pick_random_server(ServerList),
            Return_PID ! {new_server_pid, NewServer_PID},
            master_server(NumberOfGroups)
    end.

%% ----------------------------------------------------------------------------
%% @doc load_balancer/4
%% Spawns the algorithm selected by the user at the start of the simulation.
%%
%% Input: NumberOfGroups- total number of possible groups in the simulation.
%%        NumberOfServers- number of servers present in the simulation.
%%        ServerCapacity- maximum number of clients a server can hold.
%%        Algorithm- string name of the algorithm selected by the user.
%% Output: None.    
load_balancer(NumberOfGroups, NumberOfServers, ServerCapacity,
    Algorithm) ->
    File_PID = whereis(notbefore),
    GroupList = get_group_list(NumberOfGroups, NumberOfServers, [], 1),
    
    case {Algorithm} of
        {Algorithm} when Algorithm =:= "greedy" ->
            spawn(greedy, do_greedy, [GroupList, File_PID, ServerCapacity, 1,
                now()]);
        {Algorithm} when Algorithm =:= "firstworst" ->
            NewGroupList = firstworst:first_worst(GroupList, ServerCapacity,
                now()),
            io:fwrite(File_PID, "Servers,", []),
            print_file_header(File_PID, 1, NumberOfGroups),
            print_group_list(File_PID, NewGroupList, 1, NumberOfGroups)   
    end.

%% ----------------------------------------------------------------------------
%% @doc spawn_files/0
%% Spawns the before and after csv file processes and registers them so the
%% PIDs can be accessed globally.
%%
%% Input: None.
%% Output: None.    
spawn_files() ->
    OneFile = file:open("before.csv", [write]),
    FilePID = element(2, OneFile),
    register(before, FilePID),
    TwoFile = file:open("after.csv", [write]),
    AnotherPID = element(2, TwoFile),
    register(notbefore, AnotherPID).

%% ----------------------------------------------------------------------------
%% @doc print_group_list/4
%% Prints the GroupList passed into the function by the use of the function
%% print_group_count/4 to file File_PID.
%%
%% Input: File_PID- file PID of the output file printed to.
%%        GroupList- group list that is to be printed to File_PID.
%%        GroupListIterator- integer iterator that cycles through each
%%            index of the GroupList.
%%        NumberOfGroups- total number of possible groups in the simulation.
%% Output: None.
print_group_list(File_PID, GroupList, GroupListIterator, NumberOfGroups) ->
    case GroupListIterator =< length(GroupList) of
        true ->
            GroupListElement = lists:nth(GroupListIterator, GroupList),
            io:fwrite(File_PID, "Server ~w,", [GroupListIterator]),
            print_group_count(File_PID, GroupListElement, 1, NumberOfGroups),
            print_group_list(File_PID, GroupList, GroupListIterator+1, 
                NumberOfGroups);
        false ->
            ok
    end.

%% ----------------------------------------------------------------------------
%% @doc get_group_list/4
%% Compiles and returns the GroupList. For each Server_PID in the server_list,
%% a ClientList, or the clients that are on server Server_PID, is created.
%% A list of the number of clients in each group present on that server,
%% GroupCount, is then created. A final list, or the GroupListElement, is
%% appended to the running recursive list GroupList. Once every server has
%% been exhausted, the final GroupList, a list of lists of size
%% NumberOfServers, is returned.
%%
%% Input: NumberOfGroups- total number of possible groups in the simulation.
%%        NumberOfServers- number of servers present in the simulation.
%%        GroupList- initially []
%%        GroupListIterator- initially 1, allows the function to iterate
%%            through every server in the simulation.
%% Output: GroupList- final GroupList of type [[]] and size NumberOfServers.
get_group_list(NumberOfGroups, NumberOfServers, GroupList, GroupListIterator) ->
    case GroupListIterator =< NumberOfServers of
        true ->
            ServerList = ets:tab2list(server_list),
            Server_PID = element(1, lists:nth(GroupListIterator, ServerList)),
            ClientList = overseer:get_matched_clients(Server_PID),
            GroupCount = get_second_element(ClientList, [], 1),
            GroupListElement = count_groups(GroupCount, [], NumberOfGroups, 1),
            NewGroupList = GroupList ++ [GroupListElement],
            get_group_list(NumberOfGroups, NumberOfServers, NewGroupList, 
                GroupListIterator+1);
        false ->
            GroupList
    end.

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
%% to AdditionServer_PID. It then sends a message to both servers, altering each of
%% their respective states.
%%
%% Input: RemovalServer_PID- server clients are being removed from.
%%        AdditionServer_PID- server clients are being added to.
%%        Group_ID- group ID number of clients that are being moved.
%% Output: None.
handle_client_movement(RemovalServer_PID, AdditionServer_PID, Group_ID,
    Return_PID) ->
    ClientsMoved = ets:select(dictionary, [{{'$1', RemovalServer_PID, Group_ID}, 
        [], ['$$']}]),
    insert_new_client(ClientsMoved, AdditionServer_PID, Group_ID, 1),
    ClientChange = length(ClientsMoved),
    
    RemovalServer_PID ! {decrease_state, ClientChange, self()},
    receive
        {clients_removed} ->
            ok
    end,
    
    AdditionServer_PID ! {increase_state, ClientChange, self()},
    receive
        {clients_added} ->
            ok
    end,
    
    Return_PID ! {clients_moved}.

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
    

    
  
 
