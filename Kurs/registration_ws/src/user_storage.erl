-module(user_storage).
-export([add_user/2, authenticate/2, find_user/1, init_db/0, create_room/1, join_room/2, send_message/3, init_ets/0]).
-include_lib("epgsql/include/epgsql.hrl").
-define(DB_HOST, "localhost").
-define(DB_PORT, 5432).
-define(DB_NAME, "user_db").
-define(DB_USER, "root").
-define(DB_PASSWORD, "12345").

-record(chat_room, {room_name, users = [], messages = []}).

init_ets() ->
    ets:new(chat_rooms, [named_table, set, public]),
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

% Подключение пользователя к комнате
join_room(RoomName, Username) ->
    case ets:lookup(chat_rooms, RoomName) of
        [] ->
            {error, "Room does not exist"};
	[{RoomName, Room}] ->
        #chat_room{room_name = RoomName, users = Users} = Room,
            case lists:member(Username, Users) of
                true ->
                    {error, "User already in room"};
                false ->
                    UpdatedRoom = Room#chat_room{users = [Username | Users]},
                    ets:insert(chat_rooms, UpdatedRoom),
                    io:format("[DEBUG] User ~p added to Room ~p~n", [Username, RoomName]),
		    {ok, "User joined the room"}
            end;
	 _ ->
            {error, "Unexpected room data format"}
    end.

% Отправка сообщения в комнате
send_message(RoomName, Username, Message) ->
    io:format("[DEBUG] Sending message to Room: ~p by User: ~p~n", [RoomName, Username]),
    case ets:lookup(chat_rooms, RoomName) of
        [] ->
            {error, "Room does not exist"};
	[{RoomName, Room}] ->
	    #chat_room{room_name = RoomName, users = Users, messages = Messages} = Room,
		    case lists:member(Username, Users) of
			    true ->
				    NewMessage = {Username, Message},
				    UpdatedRoom = Room#chat_room{messages = [NewMessage | Messages]},
				    ets:insert(chat_rooms, UpdatedRoom),
				    {ok, "Message sent"};
			    false ->
				    {error, "User not in room"}
		    end;
	_ ->
            {error, "Unexpected room data format"}
    end.
