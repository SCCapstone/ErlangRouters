%% @doc FragmentationCalculator.
%% Contains a number of fragmenation affiliated calcuations.
%% 
%% @version 1.2
%% @TODO Explore creating functions that will complement a health-check
%%       suite implementation.
%% 
%% This uses exports from listops.erl

-module(fragcalc).
-export([servers/1, common_groups/1, group_clients/2, open_cap/2 ]).

%% ----------------------------------------------------------------------------
%% @doc servers/1
%%
%% This function calls get_all_server_frag/3, taking in a GroupList
%% and outputting a corresponding list of integers, with each 
%% integer value indicating that server's degree of fragmentation, 
%% in terms of how many other servers it must communicate with.
%% 
%%
servers(GroupList) ->
    OutputList = get_all_server_frag(GroupList, [], 1),
    OutputList.



%% ----------------------------------------------------------------------------
%% @doc get_all_server_frag/3
%%
%% This function determines the fragmentation for each server
%% respective to the the other servers within GroupList.
%% It takes in the GroupList, a BlankList ([]) to be built recursively, 
%% and a default Iterator of 1. i.e get_all_server_frag(Grouplist, [], 1)
%% 
%% Runtime = #Servers * (#Servers * #Groups) - get_server_frag
get_all_server_frag(GroupList, BlankList, Iterator) 
    when Iterator =< length(GroupList) ->
        TargetIndex = Iterator,
        FragCount = get_server_frag(GroupList, TargetIndex, 0, 1),
        NList = lists:append([BlankList, [FragCount]]),
        get_all_server_frag(GroupList, NList, Iterator + 1);
get_all_server_frag(GroupList, BlankList, Iterator) ->
    ResultList = BlankList,
    ResultList.

%% ----------------------------------------------------------------------------
%% @doc get_server_frag/4
%%
%% This function determines the fragmentation of the server corresponding
%% to TargetIndex within GroupList. 
%% We count each instance in which it has at least one group in common with
%% another server, to give us a final number of total server fragmentaion
%% for the server corresponding to TargetIndex.
%%
%% Runtime Count = #Servers *  #Groups(get_common_clients)
get_server_frag(GroupList, TargetIndex, FragCount, Iterator) 
    when Iterator =< length(GroupList) ->
        TargetServer = lists:nth(TargetIndex, GroupList),
        OtherServer = lists:nth(Iterator, GroupList),
        InCommonList = get_common_clients(TargetServer, OtherServer, [], 1),
        InCommonNum = listops:sum(InCommonList),
    case (TargetIndex =/= Iterator) and (InCommonNum > 0) of
        true ->
            get_server_frag(GroupList, TargetIndex, FragCount+1, Iterator+1); 
        false ->
            get_server_frag(GroupList, TargetIndex, FragCount, Iterator+1)
    end;
get_server_frag(GroupList, TargetIndex, FragCount, Iterator) ->
    TargetFrag = FragCount,
    TargetFrag.

%% ----------------------------------------------------------------------------
%% @doc get_common_clients/4
%%
%% This function compares the distribution of clients between two servers
%% to determine if they have an groups in common. 
%% We output an index list of the groups they have in common
%% 
%% Runtime count = #Groups
get_common_clients(ServerA, ServerB, InCommon, Iterator) 
    when Iterator =< length(ServerA) ->
        ElementA = lists:nth(Iterator, ServerA),
        ElementB = lists:nth(Iterator, ServerB),
        ClientTotal = ElementA + ElementB,
    case (ElementA =/= 0) and ( ElementB =/= 0) of
        true ->
            NewCommon = lists:append([InCommon, [ClientTotal]]);
        false -> 
            NewCommon = lists:append([InCommon, [0]])        
    end,    
    get_common_clients(ServerA, ServerB, NewCommon, Iterator+1);  
get_common_clients(ServerA, ServerB, InCommon, Iterator) ->
    ResultList = InCommon,
    ResultList.


%% ----------------------------------------------------------------------------
%% @doc common_groups/1
%%
%% Calls get_common_groups
common_groups(GroupList) ->
    SampleServer = lists:nth(1,GroupList),
    NumberofGroups = length(SampleServer),
    OutputList = get_common_groups(GroupList, [], NumberofGroups, 1),
    OutputList.


%% ----------------------------------------------------------------------------
%% @doc get_common_groups/4
%%
%% This function compares the distribution of clients between two servers
%% to determine if they have any groups in common. 
%% We output an index list of the groups they have in common
%% 
%% Runtcount = #Groups * #severs(get_group_frag)
get_common_groups(GroupList, CommonGList, NumberofGroups, Iterator) 
    when Iterator =< NumberofGroups ->
        NewValue = get_group_frag(GroupList, Iterator, 0, 1),
        NewCommon = lists:append([CommonGList, [NewValue]]),
        get_common_groups(GroupList, NewCommon, NumberofGroups, Iterator+1);  
get_common_groups(GroupList, CommonGList, NumberofGroups, Iterator) ->
    CommonGList.

%% ----------------------------------------------------------------------------
%% @doc get_group_frag/4
%%
%% Tells us how many servers have members of a group.
%%  
%%  
%% Runtime count = #Servers
get_group_frag(GroupList, GroupIndex, TotalFrag, Iterator)
    when Iterator =< length(GroupList) ->
        CurrentServer = lists:nth(Iterator, GroupList),
        ClientNum = lists:nth(GroupIndex, CurrentServer),
    case ClientNum > 0 of
        true ->
            get_group_frag(GroupList, GroupIndex, TotalFrag+1, Iterator+1);
        false -> 
            get_group_frag(GroupList, GroupIndex, TotalFrag, Iterator+1)
    end;
get_group_frag(GroupList, GroupIndex, TotalFrag, Iterator) ->
    TotalFrag.


%% ----------------------------------------------------------------------------
%% @doc clients/2
%%
%%
%%
group_clients(GroupList, GroupIndex) ->
    OutputValue = get_group_clients(GroupList, GroupIndex, 0, 1),
    OutputValue.


%% ----------------------------------------------------------------------------
%% @doc get_group_clients/4
%%
%% Tells us how many clients are in a group.
%%  
%%  
%% Runtime count = #Servers
get_group_clients(GroupList, GroupIndex, TotalClients, Iterator)
    when Iterator =< length(GroupList) ->
    CurrentServer = lists:nth(Iterator, GroupList),
    CurrentTotal = lists:nth(GroupIndex, CurrentServer) + TotalClients,
    get_group_clients(GroupList, GroupIndex, CurrentTotal, Iterator+1);
get_group_clients(GroupList, GroupIndex, TotalClients, Iterator) ->
    TotalClients.



%% ----------------------------------------------------------------------------
%% @doc open_cap/2
%% quick way to get server capacity via Grouplist and save some space 
%% elsewhere
%%
%% runtime count = #Groups
open_cap(Server, Capacity) ->
    Allocated = listops:sum(Server),
    Remaining = Capacity - Allocated,
    Remaining.


reassign_group(GroupList, GroupIndex, ServerIndex, ReceiveServer, Iterator)
    when Iterator =< length(GroupList) ->
    CurrentServer = lists:nth(Iterator, GroupList),
    case Iterator == ServerIndex of
        true ->
            NewGL = listops:change(GroupList, Iterator, ReceiveServer);
        false ->
            ClearServer = listops:change(CurrentServer, GroupIndex, 0),
            NewGL = listops:change(GroupList, Iterator, ClearServer)
    end,
    reassign_group(NewGL, GroupIndex, ServerIndex, ReceiveServer, Iterator +1);
reassign_group(GroupList, GroupIndex, ServerIndex, ReceiveServer, Iterator) ->
    GroupList.
