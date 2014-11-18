-module(overseer).
-export([main/0]).
-import(simulator, [spawnServers/2]).

main() ->  
  %%N = list_to_integer(atom_to_list(lists:nth(1, Arg)),
  %%C = list_to_integer(atom_to_list(lists:nth(2, Arg)),
  NumberOfClients = 100,
  NumberOfServers = 10,
  
  io:format("Number of clients: ~w~n", [NumberOfClients]),
  io:format("Number of servers: ~w~n", [NumberOfServers]),
  TempServerList = [],
  
  %%spawnServers(numberOfServers, numberOfServers),
  ServerList = spawnServers(NumberOfServers, TempServerList).
 
