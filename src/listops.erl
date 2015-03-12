%% @doc ListOps.
%% Various functions that operate on Lists in Erlang.
%% Does not call anything, only exports functions.
%% 
%% @version 1.0
%% @TODO 
%%
%%  
%% Date Last Modified: 3/11/2015
%% 
%%

-module(listops).
-export([ change/3, sum/1, max_index/1]).


%% ----------------------------------------------------------------------------
%% @doc change/3
%% Calls the change_listval/5 function with input List, Index, & Value.
%% Parameters:
%% {LIST} List - the list to be changed
%% {INT} Index - The index of the value to be changed
%% {ANY} Value - The new value
change(List, Index, Value) ->
	OutputList = change_listval(List, [], Index, Value, 1),
	OutputList.


%% ----------------------------------------------------------------------------
%% @doc change_listval/5
%% Reassigns the value at a particular index position to the inputted value.
%% 
%% Parameters: 
%% {LIST} InputList - The List to be changed
%% {LIST} OutputList - Put in blank list, this will become modified list '[]'
%% {INT} Index - The index corresponding to the value that needs to be changed
%% *NOTE* We can probably make this a list as well, will need a list of new... 
%% *NOTE* ...values of equal size, function could alter many values at once.
%% {ANY} Value - The value that will overrwrite the previous one.
%% {INT} Iterator - Set to 1, permits list navigation in Erlang
%% 
%% Runtime count = length of InputList
%% It might be possible to speed this up signficantly via concatenation.
%%
change_listval(InputList, OutputList, Index, Value, Iterator)
	when Iterator =< length(InputList) ->
	case Iterator == Index of
		true -> 
			NewOutputList = lists:append([OutputList, [Value]]);
		false ->
			OldVal = lists:nth(Iterator, InputList),
			NewOutputList = lists:append([OutputList, [OldVal]])
	end,
	change_listval(InputList, NewOutputList, Index, Value, Iterator+1);
change_listval(InputList, OutputList, Index, Value, Iterator) ->
	OutputList.






%% ----------------------------------------------------------------------------
%% @doc sum/1
%% Calls the sum_integers function with the inputted list.
sum(List) ->
	Sum = sum_integers(List, 0, 1),
	Sum.

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
sum_integers(List, Sum, Iterator) 
	when Iterator =< length(List) ->
	CurrentSum = lists:nth(Iterator, List),
	sum_integers(List, Sum + CurrentSum, Iterator + 1);
sum_integers(List, Sum, Iterator) ->
	Sum.  



%% ----------------------------------------------------------------------------
%% @doc max_index/1
%%
%% Calls indexof_maxvalue and gets the index first highest value for 
%% the inputted list.
max_index(List) ->
	OutputIndex = indexof_maxvalue(List, 1, 1),
	OutputIndex.




%% ----------------------------------------------------------------------------
%% @doc indexof_maxvalue/3
%% Determines which index of inputted list holds the highest value.
%% Currently used to find the index of highest fragementation value
%% which will correspond to that server in GroupList.
%% Usage: indexof_maxvalue(List,1,1) will find the index of the 
%% first highest value in List. 
%%
indexof_maxvalue(List, MaxIndex, Iterator) 
	when Iterator =< length(List) ->
	CurrentValue = lists:nth(Iterator, List),
	MaxValue =  lists:nth(MaxIndex, List),
	case CurrentValue > MaxValue of
		true -> %%Change to >= if you want last highest. 
			indexof_maxvalue(List, Iterator, Iterator + 1); 
		false ->
			indexof_maxvalue(List, MaxIndex, Iterator + 1)
	end;
indexof_maxvalue(List, MaxIndex, Iterator) ->
	TheMAX = MaxIndex,
	TheMAX.
