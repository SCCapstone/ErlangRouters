-module(load_balancer).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [];
behaviour_info(_) ->
    undefined.
