-module(list2).
-export([generate/2, m_time/1, sor_generate/1]).
-include_lib("eunit/include/eunit.hrl").

m_time(Fun) ->
        StartTime = erlang:monotonic_time(),
        Result = Fun(),
        EndTime = erlang:monotonic_time(),
        ElapsedTime = erlang:convert_time_unit(EndTime - StartTime,native,microsecond),
        io:format("Exe time: ~p microsec ~n",[ElapsedTime]),
        Result.


generate(N, Max) ->
	rand:seed(exsplus, erlang:timestamp()),
	generate_list(N,Max).

generate_list(0, _) ->
	[];
generate_list(N, Max) ->
	Random = rand:uniform(Max),
	[Random|generate_list(N-1,Max)].

sor_generate([])  -> [];
sor_generate([Pivot|Rest]) ->
        sor_generate([X||X <- Rest, X=<Pivot]) ++
        [Pivot]++
        sor_generate([X ||X <- Rest,X>Pivot]).

