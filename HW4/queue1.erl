-module(queue1).
-export([new/0, enqueue/2, dequeue/1, top/1, isEmpty/1]).
-include_lib("eunit/include/eunit.hrl").
-type queue() :: list().
-spec new() -> queue().
new() ->
	[].

-spec enqueue(Element::any(), queue()) -> queue().
enqueue(Element, Queue) ->
	Queue ++ [Element].

-spec dequeue(Queue::list()) ->
        { Head::any(), Tail::list()}|{error,empty_queue}.
dequeue([]) ->
        {error, empty_queue};
dequeue([Head|Tail]) ->
        {Head,Tail}.

-spec top(Queue::list()) -> {Head::any()}|{error,empty_queue}.
top([]) ->
        {error, empty_queue};
top([Head| _Tail]) ->
        Head.

-spec isEmpty(Queue::list()) -> boolean().
isEmpty(Queue) ->
        Queue =:=[].

new_test() ->
        NewQueue = new(),
        ?assertEqual([],NewQueue),
        ok.
enqueue_test() ->
        NewQueue = new(),
        S1 = enqueue(4, NewQueue),
        S2 = enqueue(5, S1),
        [Top | _] = S2,
        ?assertEqual(4,Top),
        ok.

dequeue_test() ->
        NewQueue = new(),
        S1 = enqueue(4, NewQueue),
        S2 = enqueue(5, S1),
        {Top,Bottom} = dequeue(S2),
        ?assertEqual(4,Top),
        ?assertEqual([5],Bottom),
        ok.

dequeue1_test() ->
        NewQueue = new(),
	{Top,Bottom} = dequeue(NewQueue),
        ?assertEqual({error, empty_queue}, {Top,Bottom}),
        ok.

top_test() ->
        NewQueue = new(),
        S1 = enqueue(4, NewQueue),
        S2 = enqueue(5, S1),
        ?assertEqual(4, top(S2)),
        ok.

top1_test() ->
        NewQueue = new(),
        ?assertEqual({error, empty_queue},top(NewQueue)),
        ok.

isEmpty_test() ->
        NewQueue = new(),
        ?assertEqual(true,isEmpty(NewQueue)),
        ok.

isEmpty1_test() ->
        NewQueue = new(),
        S1 = enqueue(7,NewQueue),
        ?assertEqual(false,isEmpty(S1)),
        ok.

