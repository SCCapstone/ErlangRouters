%% @doc Greedy.
%% Greedy algorithm for sorting clients on the cluster of servers by Group
%% ID number.
%% @version 1.1
%% @TODO Increase robustness of Greedy solution.
%% Date Last Modified: 2/10/2015

-module(greedy).
-export([do_greedy/4, reassign_clients/5, move_clients/4, change_element/3]).

%% ----------------------------------------------------------------------------
%% @doc do_greedy/5
%% This function runs the function reassign_clients/5 recursively for every member
%% of GroupList, which essentially runs our greedy algorithm locally for each
%% server in the cluster. The final modified list of load balanced clients is
%% returned once every server has been accounted for (GroupList).

do_greedy(GroupList, File_PID, ServerCapacity, GreedyIndex)
    when GreedyIndex =< length(GroupList) ->
    
    TempGroupList = reassign_clients(GroupList, File_PID,
        ServerCapacity, 1, GreedyIndex),
    do_greedy(TempGroupList, File_PID, ServerCapacity, GreedyIndex+1);
    
do_greedy(GroupList, File_PID, ServerCapacity, GreedyIndex) ->
    
    io:format("Greedy algorithm completed.~n"),
    GroupList.

%% ----------------------------------------------------------------------------
%% @doc reassign_clients/5
%% This function reassigns the clients to different servers using our first
%% iteration of the greedy algorithm.
%% It takes in a lists of lists GroupList, in which each element of the list
%% is the list of the number of clients in Groups 1 through NumberOfGroups
%% and the total number of elements is NumberOfServers. The current algorithm
%% compares the group values for the first Server in the list (FirstServerList)
%% with every other Server in GroupList (CurrentServerList). The method is
%% recursively called to compare Server 1 to every other Server, one Server
%% at a time. The function reassigns the clients by calling move_clients,
%% which runs our greedy solution and returns both Server lists that have
%% been changed in a list-tuple MovedClients. Then, the GroupList is
%% modified to account for these two changes by the use of the function
%% change_element. The function is then recursively called to take a look
%% at the next Server in the list (Servers 2 through Server NumberOfServers).
%% Once every Server in GroupList has been accounted for, reassign_clients
%% returns the final version of GroupList.
reassign_clients(GroupList, File_PID, ServerCapacity, GroupListIndex,
    GreedyIndex)
    when GroupListIndex =< length(GroupList) ->
    
    case GroupListIndex == GreedyIndex of
        true ->
            reassign_clients(GroupList, File_PID, ServerCapacity,
                GroupListIndex+1, GreedyIndex);
        false ->
            GreedyServerList = lists:nth(GreedyIndex, GroupList),
            NonGreedyServerList = lists:nth(GroupListIndex, GroupList),
            
            ServerList = ets:tab2list(server_list),
            GreedyServer_PID = element(1, lists:nth(GreedyIndex, ServerList)),
            NonGreedyServer_PID = element(1, lists:nth(GroupListIndex, 
                ServerList)),
            RelevantServers = [GreedyServer_PID, NonGreedyServer_PID],
            
            MovedClients = move_clients(GreedyServerList, NonGreedyServerList, 
                1, RelevantServers),
                
            UpdatedGreedyList = lists:nth(1, MovedClients),
            UpdatedNonGreedyList = lists:nth(2, MovedClients),
            
            UpdatedGroupList = change_element(GroupListIndex, change_element(
                GreedyIndex, GroupList, UpdatedGreedyList), UpdatedNonGreedyList),
                
            reassign_clients(UpdatedGroupList, File_PID, ServerCapacity,
                GroupListIndex+1, GreedyIndex)
    end;
reassign_clients(GroupList, File_PID, ServerCapacity, GroupListIndex, 
    GreedyIndex) ->
    GroupList.

%% ----------------------------------------------------------------------------
%% @doc move_clients/4
%% This function takes in the list information from the two servers to be
%% compared from reassign_clients and recursively runs our greedy algorithm
%% on those two lists from the first group index of the group lists to the
%% length(FirstServerList) index (last index) of the group lists. For each
%% index in these lists, the following happens: if the first (primary priority) server
%% list's element is lessthanequal to the current (secondary priority) server list's
%% element and the primary server list's value at that index is not zero,
%% then those clients in that group from the primary server are moved to
%% the secondary server by the use of the function change_element. If the
%% opposite occurs, then the clients in that particular index are moved from
%% the secondary server to the primary server. If the primary server list's
%% value at that particular index is 0, then we know that the optimum solution
%% has already been reached (no clients of a particular group ID are on the
%% server that the greedy solution is trying to optimize) so we skip that
%% index. Once every index of these group lists have been exhaused, the
%% updated primary and secondary server group lists are returned in a list
%% of size two [primary, secondary].
move_clients(GreedyServerList, NonGreedyServerList, Iterator, RelevantServers)
    when Iterator =< length(GreedyServerList) ->
    
    GreedyElement = lists:nth(Iterator, GreedyServerList),
    NonGreedyElement = lists:nth(Iterator, NonGreedyServerList),
    
    GreedyServer_PID = lists:nth(1, RelevantServers),
    NonGreedyServer_PID = lists:nth(2, RelevantServers),
    
    case {GreedyElement, NonGreedyElement} of
    
        {GreedyElement, NonGreedyElement} 
            when (GreedyElement =< NonGreedyElement) and (GreedyElement =/= 0) ->
            
                TempNonGreedyList = change_element(Iterator, NonGreedyServerList,
                    GreedyElement+NonGreedyElement),
                TempGreedyList = change_element(Iterator, GreedyServerList,
                    GreedyElement-GreedyElement),
                
                overseer:handle_client_movement(GreedyServer_PID, 
                    NonGreedyServer_PID, Iterator),
                    
                move_clients(TempGreedyList, TempNonGreedyList, Iterator+1,
                    RelevantServers);
            
        {GreedyElement, NonGreedyElement} 
            when (NonGreedyElement < GreedyElement) and (GreedyElement =/= 0) ->
            
                TempGreedyList = change_element(Iterator, GreedyServerList,
                    GreedyElement+NonGreedyElement),
                TempNonGreedyList = change_element(Iterator, NonGreedyServerList,
                    NonGreedyElement-NonGreedyElement),
                
                overseer:handle_client_movement(NonGreedyServer_PID, 
                    GreedyServer_PID, Iterator),
                    
                move_clients(TempGreedyList, TempNonGreedyList, Iterator+1,
                    RelevantServers);
                
        {GreedyElement, NonGreedyElement} 
            when GreedyElement == 0 ->
            
                move_clients(GreedyServerList, NonGreedyServerList, Iterator+1,
                    RelevantServers)
    end;
    
move_clients(GreedyServerList, NonGreedyServerList, Iterator, RelevantServers) ->
    MovedClients = [GreedyServerList, NonGreedyServerList],
    MovedClients.

%% ----------------------------------------------------------------------------
%% @doc change_element/3
%% This function replaces the element of a List at index Index with
%% NewElement through list manipulation.
%% change_element(Index, List, NewElement) -> List.
change_element(1, [_|After], NewElement) ->
    [NewElement|After];
change_element(I, [Before|After], NewElement) ->
    [Before|change_element(I-1, After, NewElement)].
