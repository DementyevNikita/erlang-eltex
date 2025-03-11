-module(ws_client_tests).
-include_lib("eunit/include/eunit.hrl").

-define(HOST, "localhost").
-define(PORT, 8080).
-define(TOKEN, "cWF6OjQ0NA==").

connect_test_() ->
    {"Successful WebSocket connection", 
     ?_test(fun() ->
         Result = ws_client:connect(?HOST, ?PORT, ?TOKEN),
         ?assertMatch({ok, _, _}, Result)
     end)}.

register_user_test() -> 
	Username = <<"qaz">>,
	Password = <<"444">>,
	Result = http_client:register_user(Username, Password),
	?assertMatch(ok, Result).

authenticate_user_test() ->
	Username = <<"qaz">>,
        Password = <<"444">>,
        Result = auth_client:authenticate_user(Username, Password),
        ?assertMatch(ok, Result).

send_message_user_test_() ->
	?_test(fun() ->
	    {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
	    Result = ws_client:send_message_user(Con,Ref,<<"asd">>, <<"privet">>),
            ?assertMatch(ok, Result)
	       end).

create_room_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:create_room(Con,Ref,<<"Room1">>),
            ?assertMatch(ok, Result)
	       end).

join_room_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:join_room(Con,Ref,<<"Room1">>),
            ?assertMatch(ok, Result)
               end).

add_friend_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:add_friend(Con,Ref,<<"asd">>),
            ?assertMatch(ok, Result)
               end).
find_user_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:find_user(Con,Ref,<<"asd">>),
            ?assertMatch(ok, Result)
               end).

find_user_friends_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:find_user_friends(Con,Ref,<<"asd">>),
            ?assertMatch(ok, Result)
               end).

offline_message_test_() ->
        ?_test(fun() ->
            {ok, Con,Ref} = ws_client:connect(?HOST, ?PORT, ?TOKEN),
            Result = ws_client:offline_message(Con,Ref),
            ?assertMatch(ok, Result)
               end).
