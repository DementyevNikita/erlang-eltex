-module(lists1).
-export([users/0, users1/0, users2/0, users3/0, spisok/0, spisok1/0, spisok2/0,users_acc/2]).

users() ->
	[{user,1,"Sasha", male, 23},
	 {user,2,"Misha", male, 27},
	 {user,3,"Lida", female, 35},
	 {user,4,"Bob", male, 18},
	 {user,5,"Lera", female, 21}].



users1() ->
	[User || {user, _,_, Gender, _} = User <- users(), Gender =:=male].

users2() ->
	[Name || {user, _,Name,_, _} <- users()].

users3() ->
	[User || {user, Id,_,_, _} = User <- users(),Id > 2 andalso Id < 5 ].

spisok() -> 
	[1,2,5,4,7,2,2,7,9].
spisok1() ->
        [0,4,6,2,1,8,0,5,5].


spisok2()->
       [X * Y || X<-spisok(), X > 4, Y <- spisok(), Y < 4].


