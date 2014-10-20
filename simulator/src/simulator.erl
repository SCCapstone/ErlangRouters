%% @doc This is our initial simulation material.
%% @version 0.0
%% @TODO A lot, presumably
-module(simulator).

-export([main/1,server/1,client/1,start/1,spawn_n/2]).


%% ----------------------------------------------------------------------------
%% @doc main
%% @end
%% 
main([Arg]) ->
  N = list_to_integer(atom_to_list(Arg)),
  start(N),
  init:stop().



%% ----------------------------------------------------------------------------
%% @doc Server with state to record number of times called. 
%% 
server(State) ->
  receive
    {request, Return_PID} ->
      io:format("Server ~w: Client request received from ~w~n",
          [self(), Return_PID]),
      NewState = State + 1,
      Return_PID ! {hit_count, NewState},
      server(NewState)
  end.



%% ----------------------------------------------------------------------------  
%% @doc Client.
%% Takes server PID as parameter, sends request and prints out value
%%
client(Server_Address) ->
  Server_Address ! {request, self()},
  receive
    {hit_count, Number} ->
      io:format("Client ~w: Hit count was ~w~n", [self(), Number])
  end.



%% ----------------------------------------------------------------------------  
%% @doc Start;Initiate test with servers and client.
%%
start(N) ->
  Server_PID = spawn(simulator,server,[0]),
  spawn_n(N ,Server_PID).

  

%% ----------------------------------------------------------------------------
%% @doc Spawn clients
%% 
spawn_n(N, Server_PID) ->
  if
    N>0 ->
      spawn(simulator,client,[Server_PID]),
      timer:sleep(random:uniform(100)),
      spawn_n(N-1,Server_PID);
    N == 0 ->
      io:format("Last client spawned. ~n")
  end.
