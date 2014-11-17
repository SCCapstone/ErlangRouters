%% @doc This is our initial simulation material.
%% @version 0.2
%% @TODO Add proper group functionality.
%% Date Last Modified: 
-module(simulator).

-export([main/0,server/1,client/2,start/1,spawn_n/2,spawnServers/1,decrement/1]).


%% ----------------------------------------------------------------------------
%% @doc main.
%% 
main() ->
  XnumServers = 10,
  spawnServers(XnumServers),
  init:stop().



%% ----------------------------------------------------------------------------
%% @doc server.
%% Maintains a state to record the number of times it is called.
server(State) ->
  receive 
    {request, Return_PID} ->
      io:format("Server ~w: Client request received from ~w~n.",
          [self(), Return_PID]),
      NewState = State + 1,
      Return_PID ! {hit_count, NewState},
      server(NewState)
    %{request, Return_PID, Group} when Group =/= Group_ID ->
      %createServer(Group),
      %server(0, Group)
  end.



%% ----------------------------------------------------------------------------  
%% @doc client(Server_Address).
%% Takes server PID as parameter, sends request and prints out value.
%%
client(Server_Address, Group) ->
  Server_Address ! {request, self(), Group},
  io:format("For client number ~w, Group_ID: ~w~n", [self(), Group]),
  receive
    {hit_count, Number} ->
      io:format("Client ~w: Hit count was ~w, Group_ID: ~w~n", 
         [self(), Number, Group])
  end.



%% ----------------------------------------------------------------------------  
%% @doc start.
%% Initiate test with servers and client.
%%
start(N) ->
  %%Group_ID = random:uniform(5),
  Server_PID = spawn(simulator,server,[0, groupList]),
  spawn_n(N ,Server_PID).

  

%% ----------------------------------------------------------------------------
%% @doc spawn_n. 
%% Spawns clients
spawn_n(N, Server_PID) ->
  if
    N>0 ->
      spawn(simulator,client,[Server_PID, random:uniform(5)]),
      timer:sleep(random:uniform(100)),
      spawn_n(N-1,Server_PID);
    N == 0 ->
      io:format("Last client spawned. ~n")
  end.


%% ----------------------------------------------------------------------------
%% @doc spawn_n. 
%% Spawns a new server. Not yet implemented, still trying to decide
%% when best to use this.
spawnServers(XnumServers) when XnumServers > 0 ->
	io:format("Number of servers left to spawn: ~w.~n", [XnumServers]),
	Server_PID = spawn(simulator, server, [0]),
	%%[Server_PID | spawnServers(decrement(XnumServers))];
		
spawnServers(0) -> 
  [],
	io:format("All servers spawned.~n").

decrement(X) -> 
	X - 1.
%%Algorithm: Take into account the capacity on each server. 
