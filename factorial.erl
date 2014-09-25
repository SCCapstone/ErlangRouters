-module(factorial).
-export([fac/1, mult/2, double/1, convert_length/1]).

fac(1) ->
	1;
fac(N) ->
	N * fac(N - 1).

mult(X,Y) ->
	X * Y.
	
double(D) ->
	2 * D.
	
convert_length({centimeter, X}) ->
	{inch, X / 2.54};
convert_length({inch, Y}) ->
	{centimeter, Y * 2.54}.
	