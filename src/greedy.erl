%% @doc Greedy.
%% Greedy algorithm for sorting clients on the cluster of servers by Group
%% ID number.
%% @version 1.3.1
%% @TODO Continue test cases for Greedy algorithm. 

-module(greedy).
-export([do_greedy/4, reassign_clients/5, move_clients/5]).

%% ----------------------------------------------------------------------------
%% @doc do_greedy/4
%% This function calls reassign_clients/5 for the particular GroupList. The
%% greedy algorithm is run and the updated GroupList is received from the
%% reassign_clients/5 process. This function runs length(GroupList) times,
%% so each server in the cluster can act as the greedy server. Once every
%% server has had its chance to 'be greedy', the total elapsed time for the
%% algorithm is printed and the final GroupList is sent to the master server
%% to be printed to csv output for the visualization piece.
do_greedy(GroupList, ServerCapacity, GreedyIndex, StartTime)
    when GreedyIndex =< length(GroupList) ->
    
    spawn(greedy, reassign_clients, [GroupList, ServerCapacity, 1, 
        GreedyIndex, self()]),
    
    receive
        {updated_group_list, TheGroupList} ->
            TempGroupList = TheGroupList 
    end,
    
    do_greedy(TempGroupList, ServerCapacity, GreedyIndex+1, StartTime);
    
do_greedy(GroupList, ServerCapacity, GreedyIndex, StartTime) ->
    EndTime = now(),
    ElapsedTime = timer:now_diff(EndTime, StartTime),
    io:format("Greedy algorithm completed.~n"),
    io:format("Elapsed time: ~w microseconds.~n", [ElapsedTime]),
    FragCalc = fragcalc:common_groups(GroupList),
    io:format("Group fragmentation post-greedy allocation: ~n~w~n", [FragCalc]),
    Master_PID = whereis(master_server),
    Master_PID ! {print_group_list, GroupList}.

%% ----------------------------------------------------------------------------
%% @doc reassign_clients/6
%% This function essentially calls the move_clients/5 function for every 
%% combination of GreedyServer/NonGreedyServer pairs (called NumberOfServers-1
%% times). GroupListIndex is the index of the overall GroupList that represents
%% the non greedy server's individual GroupList; GreedyIndex represents the
%% greedy server's individual GroupList in the overall GroupList. When the
%% GroupListIndex is not equal to the GreedyIndex (you're currently not
%% examining the greedy server's GroupList twice), the function pulls the
%% two elements of the overall GroupList from the indexes denoted by 
%% GroupListIndex and GreedyIndex, the overall ServerList is pulled from
%% global storage and the server PIDs of the two servers that are to be
%% examined by move_clients/4 are placed into RelevantServers. Then,
%% move_clients/4 is called for those two servers and the resultant updated
%% server lists are inserted into the overall GroupList. The function is
%% then run for the next possible GreedyServer/OtherServer pair.
reassign_clients(GroupList, ServerCapacity, GroupListIndex,
    GreedyIndex, Return_PID)
    when GroupListIndex =< length(GroupList) ->
    
    case GroupListIndex == GreedyIndex of
        true ->
            reassign_clients(GroupList, ServerCapacity,
                GroupListIndex+1, GreedyIndex, Return_PID);
        false ->
            GreedyServerList = lists:nth(GreedyIndex, GroupList),
            NonGreedyServerList = lists:nth(GroupListIndex, GroupList),
            
            ServerList = ets:tab2list(server_list),
            GreedyServer_PID = element(1, lists:nth(GreedyIndex, ServerList)),
            NonGreedyServer_PID = element(1, lists:nth(GroupListIndex, 
                ServerList)),
            RelevantServers = [GreedyServer_PID, NonGreedyServer_PID],
            
            spawn(greedy, move_clients, [GreedyServerList, NonGreedyServerList, 
                1, RelevantServers, self()]),
                
            receive
                {moved_clients, MovedClients} ->
                    UpdatedGreedyList = lists:nth(1, MovedClients),
                    UpdatedNonGreedyList = lists:nth(2, MovedClients)
            end,    
            
            UpdatedGroupList = listops:change_element(GroupListIndex, 
                listops:change_element(GreedyIndex, GroupList, UpdatedGreedyList), 
                UpdatedNonGreedyList),
                
            reassign_clients(UpdatedGroupList, ServerCapacity,
                GroupListIndex+1, GreedyIndex, Return_PID)
    end;
reassign_clients(GroupList, ServerCapacity, GroupListIndex, 
    GreedyIndex, Return_PID) ->
    Return_PID ! {updated_group_list, GroupList}.

%% ----------------------------------------------------------------------------
%% @doc move_clients/5
%% This function moves clients in the same group in one of two ways: 1) 
%% clients in GreedyServer -> NonGreedyServer or 2) clients in 
%% NonGreedyServer -> GreedyServer. The function takes in the two lists
%% GreedyServerList (Greedy server's individual GroupList) and
%% NonGreedyServerList(Non-greedy server's individual GroupList). The list of
%% two elements, RelevantServers, denotes which two servers these two lists are
%% referring to. For each index in these GroupLists (function iterates
%% NumberOfGroups times by incrementing Iterator), the function first sends
%% a message to both servers requesting their current states and places
%% the amount of capacity they have left in two variables GreedyRemCapacity
%% and NonGreedyRemCapacity. Then, the two lists are compared at the 
%% particular index Iterator by setting GreedyElement equal to the 
%% GreedyServerList's value and NonGreedyElement equal to NonGreedyServerList's
%% value. Then, the following cases may take place:
%%   1) GreedyElement is lessthanequal to NonGreedyElement, GreedyElement is
%%      not equal to zero. If this is the case, the clients in group number
%%      Iterator are moved from GreedyServer to NonGreedyServer, barring
%%      a lack of capacity on NonGreedyServer, in which case no clients are
%%      moved.
%%   2) GreedyElement is greater than NonGreedyElement, GreedyElement is
%%      not equal to zero. If this is the case, the clients in group number
%%      Iterator are moved from NonGreedyServer to GreedyServer, barring
%%      a lack of capacity on GreedyServer, in which case no clients are
%%      moved.
%%   3) GreedyElement is equal to zero. Because there are no clients on 
%%      GreedyServer to move, we will be 'greedy' and move on to the next
%%      group and move no clients, since no clients need to be moved.
%% Once the function has iterated through every index of GreedyServerList/
%% NonGreedyServerList, the function sends a message back to the Return_PID
%% (PID of the spawned reassign_clients function) containing MovedClients,
%% a list of two elements containing the updated GreedyServerList and 
%% NonGreedyServerList.
%%
%% Note: the bulk of the algorithmic work is done here. This function checks
%% the server capacities, moves the clients, and updates the lists accordingly.
move_clients(GreedyServerList, NonGreedyServerList, Iterator, RelevantServers,
    Return_PID) ->
    
    case Iterator =< length(GreedyServerList) of
        true -> 
            
            Master_PID = whereis(master_server),
            
            GreedyElement = lists:nth(Iterator, GreedyServerList),
            NonGreedyElement = lists:nth(Iterator, NonGreedyServerList),
            
            GreedyServer_PID = lists:nth(1, RelevantServers),
            NonGreedyServer_PID = lists:nth(2, RelevantServers),
            
            GreedyServer_PID ! {capacity_request, self()},
            
            receive
                {server_capacity, State, ServerCapacity} ->
                    GreedyRemCapacity = ServerCapacity-State
            end,
            
            NonGreedyServer_PID ! {capacity_request, self()},
            
            receive
                {server_capacity, State2, ServerCapacity2} ->
                    NonGreedyRemCapacity = ServerCapacity2-State2
            end,
            
            case {GreedyElement, NonGreedyElement} of
            
                {GreedyElement, NonGreedyElement}
                    when (GreedyElement =< NonGreedyElement) and 
                        (GreedyElement =/= 0) ->
                        
                        case NonGreedyRemCapacity >= NonGreedyElement of
                            true ->
                                TempNonGreedyList = listops:change_element(Iterator, 
                                    NonGreedyServerList, GreedyElement+
                                    NonGreedyElement),
                                TempGreedyList = listops:change_element(Iterator, 
                                    GreedyServerList,GreedyElement-
                                    GreedyElement),
                                
                                spawn(overseer, handle_client_movement,
                                    [GreedyServer_PID, NonGreedyServer_PID, 
                                    Iterator, self()]),
                                    
                                receive
                                    {clients_moved} ->
                                        ok
                                end,
                                    
                                move_clients(TempGreedyList, TempNonGreedyList, 
                                    Iterator+1, RelevantServers, Return_PID);
                            false ->
                                move_clients(GreedyServerList, NonGreedyServerList, 
                                    Iterator+1, RelevantServers, Return_PID)
                        end;
                    
                 {GreedyElement, NonGreedyElement}
                    when (NonGreedyElement < GreedyElement) and 
                        (GreedyElement =/= 0) ->
                        
                        case GreedyRemCapacity >= GreedyElement of
                            true ->
                                TempGreedyList = listops:change_element(Iterator, 
                                    NonGreedyServerList, GreedyElement+
                                    NonGreedyElement),
                                TempNonGreedyList = listops:change_element(Iterator, 
                                    GreedyServerList,NonGreedyElement-
                                    NonGreedyElement),
                                
                                spawn(overseer, handle_client_movement,
                                    [GreedyServer_PID, NonGreedyServer_PID, 
                                    Iterator, self()]),
                                
                                receive
                                    {clients_moved} ->
                                        ok
                                end,    
                                
                                move_clients(TempGreedyList, TempNonGreedyList, 
                                    Iterator+1, RelevantServers, Return_PID);
                            false ->
                                move_clients(GreedyServerList, NonGreedyServerList, 
                                    Iterator+1, RelevantServers, Return_PID)
                        end;
                 
                {GreedyElement, NonGreedyElement}
                    when (GreedyElement == 0) ->
                    
                        move_clients(GreedyServerList, NonGreedyServerList, 
                            Iterator+1, RelevantServers, Return_PID)
            end;
            
        false ->
            MovedClients = [GreedyServerList, NonGreedyServerList],
            Return_PID ! {moved_clients, MovedClients}
    end.
            
