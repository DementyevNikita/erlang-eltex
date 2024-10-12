-module(queue1).
-export([new/0, enqueue/2, dequeue/1, top/1, isEmpty/1]).

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



