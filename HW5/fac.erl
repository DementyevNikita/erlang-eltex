-module(fac).
-export([factorial/1]).

-include_lib("eunit/include/eunit.hrl").

-spec factorial(Element::int) -> int.

factorial(0) -> 1;

factorial(N) when N>0 ->
	N  * factorial(N-1).

factorial_test() ->
	S = factorial(5),
	?assertEqual(120,S),
        ok.
