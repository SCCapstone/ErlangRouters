%% @doc greedy.
%% Greedy algorithm for sorting clients on the cluster of servers by group.
%% @version 1.0
%% @TODO Implement greedy algorithm.
%% Date Last Modified: 12/02/2014

-module(greedy).
-export([reassign_clients/5, move_clients/3, change_element/3]).
-import(overseer, [count_groups/4, get_num_clients/4]).

reassign_clients(GroupList, ServerDict, File_PID, ServerCapacity, GroupListIndex) when GroupListIndex =< length(GroupList) ->
	FirstServerList = lists:nth(1, GroupList),
	CurrentServerList = lists:nth(GroupListIndex, GroupList),
	
	MovedClients = move_clients(FirstServerList, CurrentServerList, 1),
	
	MovedClients1 = lists:nth(1, MovedClients),
	MovedClients2 = lists:nth(2, MovedClients),
	
	TempList1 = change_element(1, GroupList, MovedClients1),
	TempList2 = change_element(GroupListIndex, TempList1, MovedClients2),
	
	reassign_clients(TempList2, ServerDict, File_PID, ServerCapacity, GroupListIndex+1);

reassign_clients(GroupList, ServerDict, File_PID, ServerCapacity, GroupListIndex) ->
	io:format("Clients reassigned.~n", []),
	GroupList.
	
move_clients(FirstServerList, CurrentServerList, Iterator) when Iterator =< length(FirstServerList) ->
	FirstElement = lists:nth(Iterator, FirstServerList),
	CurrentElement = lists:nth(Iterator, CurrentServerList),
	
	if
		(FirstElement =< CurrentElement) and (FirstElement =/= 0) ->
			TempCurrentList = change_element(Iterator, CurrentServerList, 
				FirstElement+CurrentElement),
			move_clients(FirstServerList, TempCurrentList, Iterator+1);
		(CurrentElement =< FirstElement) and (FirstElement =/= 0) ->
			TempFirstList = change_element(Iterator, FirstServerList, 
				FirstElement+CurrentElement),
			move_clients(TempFirstList, CurrentServerList, Iterator+1);
		FirstElement == 0 ->
			move_clients(FirstServerList, CurrentServerList, Iterator+1)
	end;
	
move_clients(FirstServerList, CurrentServerList, Iterator) ->
	MovedClients = [FirstServerList, CurrentServerList],
	MovedClients.

%% change_element(Index, List, NewElement) -> List.
change_element(1, [_|After], NewElement) -> 
	[NewElement|After];
change_element(I, [Before|After], NewElement) -> 
	[Before|change_element(I-1, After, NewElement)].	
