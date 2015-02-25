%% @doc FirstWorst.
%% 'First is worst' algorithm for sorting clients 
%% on the cluster of servers by Group ID number.
%% 
%% @version 1.0
%% @TODO implement the actual reallocation processes. 
%%
%% I have commented out the code needing changes
%% so that this module can compile. 
%% Delete all %* tokens to resume work on this. 
%% Date Last Modified: 2/22/2015
%% 
%%
-module(firstworst).
-export([sum_integers/3, indexof_maxvalue/3]).




%% ----------------------------------------------------------------------------
%% @doc first_worst/?
%% This is the overall function to do all the processes of the Algorithm.
%% It will need to take in GroupList, and probably everything imported
%% by the greed algorithim (TODO).
%%
%*first_worst(GroupList, ...) ->
	
%*	FragList = fragcalc:get_all_frag(GroupList,[],1),
%*	FragCoefficient = sum_integers(FragList,1,1), 
	%% technically we should divide Coefficient by two, it doesn't make
	%% any functional difference, but it is a good way to catch errors. 
%*	reduce_frag(GroupList, FragList, FragCoefficient, 0);
%first_worst(GroupList, ...) ->
%*	io:format("First-Worst algorithm complete.~n",[]),
%* 	GroupList.




%% ----------------------------------------------------------------------------
%% @doc reduce_frag/?
%% 
%% Do this until either all fragmentation eliminated {FCoeff == 0},
%% or until we've tried it for every server {Attempts == length(GroupList)}
%*reduce_frag(GroupList, FragList, FCoeff, Attempts) when FCoeff > 0 ->
	%% if Attempts = length(GroupList)
	%% exit by: 
%*	WorstInd = indexof_maxvalue(FragList,1,1),
	%% FragBefore = lists:nth(WorstIndex, FragList), (for more detail)
%*	UpdatedGroupList = purge_server(GroupList, WorstInd, *ServerCapacity*, 1),
	%% FragAfter = lists:nth(WorstIndex, FragList),
%*	UpdatedFragList = fragcalc:get_all_frag(UpdatedGroupList,[],1),
%*	UpdatedCoeff = sum_integers(UpdatedFragList,1,1),
%*	reduce_frag(UpdatedGroupList, UpdatedFragList, UpdatedCoeff, Attempts +1);
%*reduce_frag(GroupList, FragList, FCoeff, Attempts) ->
%*	GroupList.


%% ----------------------------------------------------------------------------
%% @doc purge/?
%% This will attempt to eliminate all fragmentation on the server corresponding
%% to the inputted index. Outputs the resulting GroupList.
%% It probably needs to somehow check 
%% whether all changes can be made before doing do
%*purge_server(GroupList, Index, *ServerCap*, Iterator) when Iterator =< length(GroupList) ->

%*purge_server(GroupList, Index, *ServerCap*, Iterator) ->
%*	GroupList.

%% ----------------------------------------------------------------------------
%% @doc sum_integers/3
%% This function determines the total sum of all integers in a list. 
%% We are using this to determine the FragCoefficient 
%% It takes in a list(List), an initial sum of 0 (Sum), 
%% and the starting Index (Iterator). ie. sum_integers(List,0,1)
%% 
%% Possible TODO: Make it take the sum up to a certain index value?
%% I kind of hesistate to do that because 
%% it's really easy break if incorrect indecies are inputted.
sum_integers(List, Sum, Iterator) when Iterator =< length(List) ->
	CurrentSum = lists:nth(Iterator, FragList),
	sum_integers(List, Sum + CurrentSum, Iterator + 1);
sum_integers(List, Sum, Iterator) ->
	TotalSum = Sum ->
	Sum.  



%% ----------------------------------------------------------------------------
%% @doc indexof_maxvalue/3
%% Determines which index of inputted list holds the highest value.
%% Currently used to find the index of highest fragementation value
%% which will correspond to that server in GroupList.
%% Usage: indexof_maxvalue(List,1,1) will find the index of the 
%% first highest value in List. 
%%
indexof_maxvalue(List, MaxIndex, Iterator) when Iterator =< length(List) ->
	CurrentValue = lists:nth(Iterator, List),
	MaxValue =  lists:nth(MaxIndex, List),
	if
		(CurrentValue > MaxValue) -> %%Change to >= if you want last highest. 
			indexof_maxvalue(List, Iterator, Iterator + 1); 
		(CurrentValue =< MaxValue) ->
			indexof_maxvalue(List, MaxIndex, Iterator + 1)
	end;
indexof_maxvalue(List, MaxIndex, Iterator) ->
	TheMAX = MaxIndex,
	TheMAX.
