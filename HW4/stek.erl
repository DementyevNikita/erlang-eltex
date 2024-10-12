-module(stek).
-export([new/0, push/2, pop/1, top/1, isEmpty/1]).

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
	{Head}.

-spec isEmpty(Stack::list()) -> boolean().
isEmpty(Stack) ->
	Stack =:=[].


