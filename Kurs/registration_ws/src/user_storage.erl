-module(user_storage).
-export([add_user/2, authenticate/2, find_user/1, init_db/0, create_room/1, join_room/2, send_message/3, init_ets/0, send_to_user/3]).
-include_lib("epgsql/include/epgsql.hrl").
-define(DB_HOST, "localhost").
-define(DB_PORT, 5432).
-define(DB_NAME, "user_db").
-define(DB_USER, "root").
-define(DB_PASSWORD, "12345").

-record(chat_room, {room_name, users = [], messages = []}).

init_ets() ->
    ets:new(chat_rooms, [named_table, set, public]),
    ets:new(user_sessions, [named_table, {keypos, 1}, {read_concurrency, true}, public]),
    ok.

init_db() ->
    {ok, Conn} = epgsql:connect(#{host => ?DB_HOST,
                                    database => ?DB_NAME,
                                    username => ?DB_USER,
                                    password => ?DB_PASSWORD,
                                    port => ?DB_PORT}),
    CreateTableQuery = "CREATE TABLE IF NOT EXISTS users (
                           username VARCHAR(255) PRIMARY KEY,
                           password VARCHAR(255) NOT NULL
                        );",
    epgsql:squery(Conn, CreateTableQuery),
    epgsql:close(Conn),
    ok.
add_user(Username, Password) ->
    {ok, Conn} = epgsql:connect(#{host => ?DB_HOST,
                                    database => ?DB_NAME,
                                    username => ?DB_USER,
                                    password => ?DB_PASSWORD,
                                    port => ?DB_PORT}),
    Query = "INSERT INTO users (username, password) VALUES ($1, $2) ON CONFLICT DO NOTHING;",
    case epgsql:equery(Conn, Query, [Username, Password]) of
        {ok, _} -> 
            epgsql:close(Conn),
            {ok, "User added"};
        {error, Reason} -> 
            epgsql:close(Conn),
            {error, Reason}
    end.
authenticate(Username, Password) ->
    {ok, Conn} = epgsql:connect(#{host => ?DB_HOST,
                                    database => ?DB_NAME,
                                    username => ?DB_USER,
                                    password => ?DB_PASSWORD,
                                    port => ?DB_PORT}),
    Query = "SELECT password FROM users WHERE username = $1;",
    case epgsql:equery(Conn, Query, [Username]) of
        {ok, _, [PasswordRow]} when PasswordRow =:= {Password} -> 
            epgsql:close(Conn),
            true;
        {ok, _, _} -> 
            epgsql:close(Conn),
            false;
        {error, Reason} -> 
            epgsql:close(Conn),
            {error, Reason}
    end.

find_user(Username) ->
    {ok, Conn} = epgsql:connect(#{host => ?DB_HOST,
                                    database => ?DB_NAME,
                                    username => ?DB_USER,
                                    password => ?DB_PASSWORD,
                                    port => ?DB_PORT}),
    Query = "SELECT username FROM users WHERE username = $1;",
    case epgsql:equery(Conn, Query, [Username]) of
        {ok, _, [{Username}]} -> 
            epgsql:close(Conn),
            {ok, Username};
        {ok, _, _} -> 
            epgsql:close(Conn),
            not_found;
        {error, Reason} -> 
            epgsql:close(Conn),
            {error, Reason}
    end.

create_room(RoomName) ->
    case ets:lookup(chat_rooms, RoomName) of
        [] ->
            Room = #chat_room{room_name = RoomName},
            case ets:insert(chat_rooms, {RoomName, Room}) of
		    true -> 
			    {ok, "Room created"};
		    false -> 
			    {error, "Failed to insert room"}
	    end;
	_ ->
            {error, "Room already exists"}
    end.

join_room(RoomName, Username) when is_binary(RoomName), is_binary(Username) ->
    case ets:lookup(chat_rooms, RoomName) of
        [] -> {error, "Room does not exist"};
        [{RoomName, Room}] ->
            #chat_room{users = Users} = Room,
            case lists:member(Username, Users) of
                true -> {error, "User already in room"}; % Пользователь уже в комнате
                false -> % Добавляем пользователя
                    UpdatedRoom = Room#chat_room{users = [Username | Users]},
                    ets:insert(chat_rooms, {RoomName, UpdatedRoom}),
                    io:format("[DEBUG] User ~p joined Room ~p~n", [Username, RoomName]),
                    {ok, "User joined the room"}
            end
    end.

send_message(RoomName, Username, NMessage) 
    when is_binary(RoomName), is_binary(Username), is_binary(NMessage) ->
    io:format("[DEBUG] Sending message to Room: ~p by User: ~p~n", [RoomName, Username]),
    case ets:lookup(chat_rooms, RoomName) of
        [] -> {error, "Room does not exist"};
        [{RoomName, Room}] ->
            #chat_room{users = Users, messages = Messages} = Room,
            case lists:member(Username, Users) of
                true -> % Добавляем сообщение
                    NewMessage = {Username, NMessage},
		    UpdatedMessages = [NewMessage | Messages],
                    UpdatedRoom = Room#chat_room{messages = UpdatedMessages},
                    ets:insert(chat_rooms, {RoomName, UpdatedRoom}),
		    lists:foreach(
                        fun(User) ->
                            case catch send_to_user(User, RoomName, NMessage) of
                                {ok, _} ->
                                    ok;
                                {error, Reason} ->
                                    io:format("[ERROR] Failed to send a message to User: ~p, Reason: ~p~n", [User, Reason]);
                                _ ->
                                    io:format("[ERROR] Unexpected error while sending to User: ~p~n", [User])
                            end
                        end,
                        Users
                    ),
                    {ok, "Message sent"};
                false -> {error, "User not in room"} 
            end
    end.

send_to_user(User, RoomName, NMessage) ->
    case ets:lookup(user_sessions, User) of
        [] ->
            io:format("[ERROR] No WebSocket connection found for user ~p~n", [User]),
            {error, "User not connected"};
        [{User, WebSocketPID}] ->
            try 
                MessageToSend = #{<<"room">> => RoomName, <<"message">> => NMessage},
                WebSocketPID ! {text, jsx:encode(MessageToSend)},
                
                io:format("[INFO] Message sent to User: ~p in Room: ~p, Message: ~p~n", [User, RoomName, MessageToSend]),
                {ok, "Message sent"}
            catch
                _:Error ->
                    io:format("[ERROR] Failed to send a message to User: ~p, Error: ~p~n", [User, Error]),
                    {error, "Failed to send message"}
            end
    end.
