%% @doc FragmentationCalculator.
%% Calculates the fragmentation for each server in a GroupList
%% ID number.
%% @version 1.0
%% @TODO 
%% Date Last Modified: 2/22/2015
%% 
%% Usage: fragcalc:get_all_frag(GroupList,[],1).
%% provides a list of integers with the same length of GroupList, 
%% each of which indicates the degree of fragmentation in the 
%% server of the same index. 

-module(fragcalc).
-export([ get_all_frag/3, get_frag/4, compare_servers/3]).


%%---------OLD CODE-------------------------
%%frag_calc/1,
%%
%%frag_calc(GroupList) ->
%%	BlankList = [],
%%	FragList = get_all_frag(GroupList, BlankList, 1);
%%frag_calc(GroupList) ->
%%	FinalList = FragList,
%%	FinalList. 
%%---------OLD CODE-------------------------


%% ----------------------------------------------------------------------------
%% @doc get_all_frag/3
%%
%% This function determines the fragmentation for each server within 
%% GroupList.
%% It takes in the GroupList, a BlankList ([]) to be built recursively, and a 
%% default Iterator of 1. i.e get_all_frag(Grouplist, [], 1)
%% 
%%
get_all_frag(GroupList, BlankList, Iterator) when Iterator =< length(GroupList) ->
	TargetIndex = Iterator,
	FragCount = get_frag(GroupList, TargetIndex, 0, 1),
	NList = lists:append([BlankList, [FragCount]]),
	get_all_frag(GroupList, NList, Iterator + 1);
get_all_frag(GroupList, BlankList, Iterator) ->
	ResultList = BlankList,
	ResultList.

%% ----------------------------------------------------------------------------
%% @doc get_frag/4
%%
%% This function determines the fragmentation of the server corresponding
%% to TargetIndex within GroupList. 
%% We count each instance in which it has at least one group in common with
%% another server, to give us a final number of total server fragmentaion
%% for the server corresponding to TargetIndex.
%%
get_frag(GroupList, TargetIndex, FragCount, Iterator) when Iterator =< length(GroupList) ->
	TargetServer = lists:nth(TargetIndex, GroupList),
	OtherServer = lists:nth(Iterator, GroupList),
	FragTest = compare_servers(TargetServer, OtherServer, 1),
	FragVektor = length(TargetServer) + length(OtherServer),
	if
		(TargetIndex == Iterator) ->
			get_frag(GroupList, TargetIndex, FragCount, Iterator + 1); %% Do nothing when comparing server against itself
		(FragTest == FragVektor) ->
			get_frag(GroupList, TargetIndex, FragCount+1, Iterator + 1); %% Increment instance of fragmentation when detected
		(FragTest =/= FragVektor) ->
			get_frag(GroupList, TargetIndex, FragCount, Iterator + 1) %% Do not increment if not dectected
	end;
get_frag(GroupList, TargetIndex, FragCount, Iterator) ->
	TargetFrag = FragCount,
	TargetFrag.

%% ----------------------------------------------------------------------------
%% @doc compare_servers/3
%%
%% This function compares the distribution of clients between two servers
%% to detrmine if they have an groups in common. 
%% If they do, we output the sum of their lengths.
%% If they don't, we output the server list length (number of groups) + 1
%%
compare_servers(ServerA, ServerB, Iterator) when Iterator =< length(ServerA) ->
  ElementA = lists:nth(Iterator, ServerA),
  ElementB = lists:nth(Iterator, ServerB),
  if
	(ElementA =/= 0) and ( ElementB =/= 0) ->
        compare_servers(ServerA, ServerB, length(ServerA) + length(ServerB));  
    (ElementA == 0) or (ElementB == 0) ->
    	compare_servers(ServerA, ServerB, Iterator + 1)
  end;
compare_servers(ServerA, ServerB, Iterator) ->
	ResultInteger = Iterator,
	ResultInteger.

  
  
