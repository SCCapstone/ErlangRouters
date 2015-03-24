%% @doc FirstWorst.
%% 'First is worst' algorithm for sorting clients 
%% on the cluster of servers by Group ID number.
%% 
%% @version 1.4
%% @TODO  
%%  *implement the actual reallocation processes. 
%%  *implement group division when capacity is insufficient
%%  *optimize fragcalc.erl & listops.erl functions
%%
%%
%% 
%% This module uses exports of fragcalc.erl & listops.erl

-module(firstworst).
-export([first_worst/3]).



%% ----------------------------------------------------------------------------
%% @doc first_worst/2
%% This is the overall function to do all the processes of the Algorithm.
%% It will need to take in GroupList, and probably everything imported
%% by the greedy algorithim (TODO). 
%% It is essentially a wrapper for expunge_frag/5
%%
%% Parameters:
%% {LIST} GroupList - The list of lists to be optimized. [Servers[Clients]]
%% {INT} Capacity - (currently) global value for capacity limit of all servers.
%%
%% Runtime expense: 
first_worst(GroupList, Capacity, StartTime) ->

    %% A list that indicates the number of servers each group has clients on.
    CommGList = fragcalc:common_groups(GroupList),

    %% Index of most fragmented group.
    WGInd = listops:max_index(CommGList),

    %% The actual degree of fragmentation for the most fragmented group.
    WFrag = lists:nth(WGInd, CommGList),

    %%Calculate and print the initial server to server fragmentation
    IFrag = fragcalc:servers(GroupList),
    io:format("~nInitial Server Fragmentation: ~w.", [IFrag]),

    %% Printing out the list of group divisions
    io:format("~nInitial Group splits of: ~w.~n ", [CommGList]),

    %% The operative step of the algorithm
    NewGroupList = expunge_frag(GroupList, CommGList, WGInd, WFrag, Capacity),
    

    RFrag = fragcalc:servers(NewGroupList),
    FCase = fragcalc:common_groups(NewGroupList),
    io:format("~nResulting Server Fragmentation: ~w.",[RFrag]),
    io:format("~nFinal Group Splits of ~w.~n~nfirstworst Algorithm complete: ~n"
        ,[FCase]),
    EndTime = now(),
    ElapsedTime = timer:now_diff(EndTime, StartTime),
    io:format("Elapsed time: ~w microseconds.~n", [ElapsedTime]),
    
    NewGroupList.



%% ----------------------------------------------------------------------------
%% @doc expunge_frag/5
%% Use the fragcalcuations to do a more 'clever' algorithim
%%
%% Parameters:
%% {LIST} GroupList - should be obvious
%% {LIST} CommGList - also pretty obvious, give me a break I am really tired.
%% {INT} WGInD - index of the current most fragmented group
%% {INT} WFrag - The actual value current worst group's fragmentation.
%% {INT} Capacity - indicates the Capacity restrictions of all servers
%%
%%  
%% We might want to run multiple passes of expunge if it's not too costly
%%
expunge_frag(GroupList, CommGList, WGInd, WFrag, Capacity)
    when WFrag > 1 ->
        %% NewGL is the recursive modification to GroupList
        GroupClients = fragcalc:group_clients(GroupList, WGInd),
        io:format("~nGroup of ~w at index ~w is split among ~w servers. ~n", 
        [GroupClients, WGInd, WFrag]),

        %%Make an attempt to reallocate the group in order to reduce frag.
        NewGL = purge_group(GroupList, WGInd, GroupClients, Capacity, 1),
        
        %% Udpate group fragmenation list regardless of what occurs in purge. 
        NewCommGList = listops:change(CommGList, WGInd, 1), 
        
        %% From there we designate the next worst group index...
        NewWGInd = listops:max_index(NewCommGList),
        
        %% ...and determine its value.
        NewWFrag = lists:nth(NewWGInd, NewCommGList),
        
        %% This continues recursively until all groups with a 
        %% fragmentation > 1 have been attempted. 
        expunge_frag(NewGL, NewCommGList, NewWGInd, NewWFrag, Capacity);
expunge_frag(GroupList, CommGList, WGInd, WFrag, Capacity) ->
    GroupList.




%% ----------------------------------------------------------------------------
%% @doc purge_group/4
%% This will attempt to eliminate all fragmentation on the group corresponding
%% to the inputted index. Outputs the resulting GroupList.
%% Currently it tries to find a server that can hold all the clients.
%% 
%%
%% Parameters:
%% {LIST} GroupList - should be obvious
%% {INT} WGInD - index of the current most fragmented group
%% {INT} GroupClients - number of clients in group
%% {INT} Capacity - indicates the Capacity restrictions of all servers
%% {INT} Iterator - duh
%%
purge_group(GroupList, WGInd, GroupClients, Capacity, Iterator)
    when Iterator =< length(GroupList) ->
    CurrentServer = lists:nth(Iterator, GroupList),
    %%io:format("Lookin at server  ~w  ~n", [CurrentServer]),
    LocalClients = lists:nth(WGInd, CurrentServer),
    ServerSpace = fragcalc:open_cap(CurrentServer, Capacity),
    RemoteClients = GroupClients - LocalClients,
    case ServerSpace >= RemoteClients of 
        true ->
            RecServ = listops:change(CurrentServer, WGInd, GroupClients),
            io:format("Success, ~w <= ~w, server ~w -> ~w.~n", 
            [RemoteClients, ServerSpace, CurrentServer, RecServ]),
            NewGL = reassign_group(GroupList, WGInd, Iterator, RecServ, 1),
            ExitInt = length(GroupList) + 12,
            purge_group(NewGL, WGInd, GroupClients, Capacity, ExitInt);
        false ->
            io:format("Failure, ~w > ~w for server ~w.~n", 
            [RemoteClients, ServerSpace, CurrentServer]),
            purge_group(GroupList, WGInd, GroupClients, Capacity, Iterator +1)
    end;
purge_group(GroupList, WGInd, GroupClients, Capacity, Iterator) ->
    GroupList.


%% ----------------------------------------------------------------------------
%% @doc reassign_Group/5
%% This function is run once a reallocation is ready to be made. 
%% It actually does the re-allocation for GroupList
%%
%%
%% Parameters:
%% {LIST} GroupList - should be obvious
%% {INT} GroupIndex - index of the group being reallocated
%% {INT} SIndex - index of the server receiving the group
%% {List} RcvServer - actual configuration of receiving server
%% {INT} Iterator - duh
%%
reassign_group(GroupList, GroupIndex, SIndex, RcvServer, Iterator)
    when Iterator =< length(GroupList) ->
    CurrentServer = lists:nth(Iterator, GroupList),
    case Iterator == SIndex of
        true ->
            NewGL = listops:change(GroupList, Iterator, RcvServer),
            reassign_group(NewGL, GroupIndex, SIndex, RcvServer, Iterator+1);
        false ->
            ServerList = ets:tab2list(server_list),
            RcvServer_PID = element(1, lists:nth(SIndex, ServerList)),
            SndServer_PID = element(1, lists:nth(Iterator, ServerList)),
            spawn(overseer, handle_client_movement,
                                    [SndServer_PID, RcvServer_PID, 
                                    GroupIndex]),            
            ClearServer = listops:change(CurrentServer, GroupIndex, 0),
            NewGL = listops:change(GroupList, Iterator, ClearServer),
            reassign_group(NewGL, GroupIndex, SIndex, RcvServer, Iterator+1)
    end;    
reassign_group(GroupList, GroupIndex, SIndex, RcvServer, Iterator) ->
    GroupList.









