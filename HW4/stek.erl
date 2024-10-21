-module(stek).
-export([new/0, push/2, pop/1, top/1, isEmpty/1]).
-include_lib("eunit/include/eunit.hrl").
-type stack() :: list(). 
-spec new() -> stack().
new() ->
	[].

-spec push(Element::any(), stack()) -> stack().
push(Element, Stack) ->
	[Element | Stack].

-spec pop(Stack::list()) -> 
	{ Head::any(), Tail::list()}|{error,empty_stack}.
pop([]) ->
        {error, empty_stack};
pop([Head|Tail]) ->
	{Head,Tail}.

-spec top(Stack::list()) -> {Head::any()}|{error,empty_stack}.
top([]) ->
	{error, empty_stack};
top([Head| _Tail]) -> 
	Head.

-spec isEmpty(Stack::list()) -> boolean().
isEmpty(Stack) ->
	Stack =:=[].

new_test() ->
	NewStack = new(),
        ?assertEqual([],NewStack),
        ok.
push_test() ->
	NewStack = new(),
        S1 = push(4, NewStack),
        S2 = push(5, S1),
	[Top | _] = S2,
        ?assertEqual(5,Top),
        ok.

pop_test() ->
	NewStack = new(),
	S1 = push(4, NewStack),
	S2 = push(5, S1),
	{Top,Bottom} = pop(S2),
	?assertEqual(5,Top),
	?assertEqual([4],Bottom),
	ok.

pop1_test() ->
        NewStack = new(),
        {Top,Bottom} = pop(NewStack),
        ?assertEqual({error, empty_stack},{Top,Bottom}),
        ok.

top_test() ->
        NewStack = new(),
        S1 = push(4, NewStack),
        S2 = push(5, S1),
        ?assertEqual(5, top(S2)),
        ok.

top1_test() ->
        NewStack = new(),
        ?assertEqual({error, empty_stack},top(NewStack)),
        ok.

isEmpty_test() -> 
	NewStack = new(),
	?assertEqual(true,isEmpty(NewStack)),
        ok.

isEmpty1_test() ->
        NewStack = new(),
	S1 = push(7,NewStack),
        ?assertEqual(false,isEmpty(S1)),
        ok.

