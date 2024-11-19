-module(factail).
-export([fac/1]).

-include_lib("eunit/include/eunit.hrl").

-spec fac(Element::int) -> int.
fac(N) when N>=0 ->
	factorial_acc(N,1).

-spec factorial_acc(Element::int, Acc::int) -> int. 
factorial_acc(0,Acc) -> Acc;
factorial_acc(N,Acc) when N>0 ->
	factorial_acc(N-1,N*Acc).

fac_test() ->
	S = fac(4),
	?assertEqual(24,S),
        ok.
