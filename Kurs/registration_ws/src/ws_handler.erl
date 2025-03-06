-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/3, websocket_handle/2, websocket_info/2, terminate/3, send_message_to_client/1, get_friends_online/1]).

init(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    io:format("WebSocket connected for user: ~p~n", [Req0]),
    Pid = maps:get(pid, Req0),
    TokenParam = proplists:get_value(<<"token">>, Qs),
    case TokenParam of
        undefined ->
            Req1 = cowboy_req:reply(401, #{}, <<"Missing or invalid token">>, Req0),
            {shutdown, Req1, State};
        TokenValue ->
            case verify_token(binary_to_list(TokenValue)) of
                {ok, Username} ->
                    io:format("WebSocket connected for user: ~p~n", [Username]),
                    ets:insert(user_sessions, {Username, Pid}),
                    {cowboy_websocket, Req0, #{username => Username}};
                {error, Reason} ->
                    Msg = io_lib:format("Invalid token: ~p", [Reason]),
                    Req1 = cowboy_req:reply(401, #{}, list_to_binary(Msg), Req0),
                    {shutdown, Req1, State}
            end
    end.

websocket_init(Req, _Opts, State) ->
    io:format("Client connected: ~p~n", [self()]),
    {Req, _Opts, State}.

websocket_handle({text, Msg}, State) ->
    Username = maps:get(username, State),
    case is_binary(Msg) of
        true ->
            case parse_message(binary_to_list(Msg)) of
                {create_room, RoomName} ->
                    case user_storage:create_room(RoomName) of
                        {ok, _} ->
                            io:format("Room ~p created by user ~p~n", [RoomName, Username]),
                            {reply, {text, <<"Room created">>}, State};
                        {error, Reason} ->
                            io:format("Room creation error for ~p: ~p~n", [RoomName, Reason]),
                            {reply, {text, <<"Error: Room already exists">>}, State}
                    end;
                {join_room, RoomName} ->
                    case user_storage:join_room(RoomName, Username) of
                        {ok, _} ->
                            io:format("User ~p joined room ~p~n", [Username, RoomName]),
                            {reply, {text, <<"Joined room successfully">>}, State};
                        {error, Reason} ->
                            io:format("Join room error for user ~p: ~p~n", [Username, Reason]),
                            {reply, {text, <<"Error: Unable to join room">>}, State}
                    end;
                {send_message, RoomName, NMessage} ->
                    case user_storage:send_message(RoomName, Username, NMessage) of
                        {ok, _} ->
			    io:format("Message sent to room ~p by user ~p:~p~n", [RoomName, Username,NMessage]),
                            {reply, {text, <<"Message sent successfully">>}, State};
                        {error, Reason} ->
                            io:format("Message error for room ~p: ~p~n", [RoomName, Reason]),
                            {reply, {text, <<"Error: Unable to send message">>}, State}
                    end;
		{custom_message, NMessage} ->
                    io:format("Custom message from ~p: ~p~n", [Username, NMessage]),
		    send_message_to_client(NMessage),
		    {reply, {text, <<"Custom message received">>}, State};
		{add_friend, FriendName} ->
                    case user_storage:add_friend(Username, FriendName) of
                        ok ->
                            io:format("User ~p added friend ~p~n", [Username, FriendName]),
                            {reply, {text, <<"Friend added successfully">>}, State};
                        {error, Reason} ->
                            io:format("Failed to add friend for ~p: ~p~n", [Username, Reason]),
                            {reply, {text, <<"Error adding friend">>}, State}
                    end;
		{find_user, User} ->
                    case user_storage:find_user(User) of
			{ok, _}  ->
                            io:format("Find user ~p~n", [User]),
                            {reply, {text, <<"User ", User/binary>>}, State};
                        {error, Reason} ->
                            io:format("Failed to add friend for ~p: ~p~n", [Username, Reason]),
                            {reply, {text, <<"Error find user">>}, State}
                    end;
		{find_user_friends, User} ->
                    case user_storage:find_user_friends(User) of
			{ok, Friends}  ->
			    {ok,Online} = get_friends_online(User),
			    OnlineBin = list_to_binary(Online),
                            io:format("Find ~p friends", [User]),
                            {reply, {text, <<"Friends: ", Friends/binary, " Online: ", OnlineBin/binary>>}, State};
                        {error, Reason} ->
                            io:format("Failed to add friend for ~p: ~p~n", [Username, Reason]),
                            {reply, {text, <<"Error find friend">>}, State}
                    end;
                {user_message, User, NMessage} ->
                    io:format("Custom message from ~p to ~p: ~p~n", [Username,User, NMessage]),
                    send_message_user(Username, User, NMessage),
                    {reply, {text, <<"message received">>}, State};
                invalid ->
                    {reply, {text, <<"Invalid command or ping">>}, State}
            end;
        false ->
            io:format("Ошибка: недопустимое сообщение от клиента: ~p~n", [Msg]),
            {reply, {text, <<"Ошибка: Сообщение не является допустимой строкой">>}, State}
   end;

websocket_handle({binary, _Msg}, State) ->
    {reply, {binary, <<"ping">>}, State};



websocket_handle(_Data, State) ->
    io:format("Неверный формат сообщения: ~p~n", [_Data]),
    {reply, {text, <<"Ошибка: Неверный формат сообщения">>}, State}.

websocket_info({custom_message, NMessage}, State) ->
       {reply, {text, NMessage}, State};

websocket_info({user_message, #{from := Sender, body := NMessage}}, State) ->
    MessageWithSender = <<"From ", Sender/binary, ": ", NMessage/binary>>,
    {reply, {text, MessageWithSender}, State};

websocket_info(_Info,State) ->
    {ok, State}.

terminate(_Reason, _Req, #{username := Username}) ->
    ets:delete(user_sessions, Username),
    io:format("WebSocket terminated for user: ~p~n", [Username]),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.

verify_token(Token) ->
    try
        Decoded = base64:decode(Token),
        [Username, Password] = binary:split(Decoded, <<":">>),
        case user_storage:authenticate(binary_to_list(Username),(Password)) of
            true ->
		io:format("Token verified. Username: ~p~n", [Username]),
                {ok, Username};
            false ->
                {error, invalid_credentials}
        end
    catch
        _:_ ->
            {error, invalid_token}
    end.

parse_message(Message) ->
    MessageBin = list_to_binary(Message),
    case binary:split(MessageBin, <<" ">>, [global]) of
        [<<"create_room">>, RoomName] -> {create_room, RoomName};
        [<<"join_room">>, RoomName] -> {join_room, RoomName};
        [<<"send_message">>, RoomName, NMessage] -> {send_message, RoomName, NMessage};
	[<<"custom_message">>, NMessage] -> {custom_message, NMessage};
	[<<"add_friend">>, FriendName] -> {add_friend, FriendName};
	[<<"find_user">>, User] -> {find_user, User};
	[<<"find_user_friends">>, User] -> {find_user_friends, User};
	[<<"user_message">>, User, NMessage] -> {user_message, User, NMessage};
	_ -> invalid
    end.

send_message_to_client(NMessage) ->
    Clients = ets:tab2list(user_sessions),
    lists:foreach(fun({_, ClientPid}) ->
                      ClientPid ! {custom_message, NMessage}
                  end, Clients).

send_message_user(Sender,User, NMessage) ->
    case ets:lookup(user_sessions, User) of
        [] ->
            io:format("[ERROR] No WebSocket connection found for user ~p~n", [User]),
            {error, "user not connected"};
        [{User, ClientPid}] ->
            try
                ClientPid ! {user_message, #{from => Sender, body => NMessage}},
                io:format("[INFO] Message sent ~p to User: ~p, Message: ~p~n", [Sender, User, NMessage]),
                {ok, "Message sent"}
            catch
                Error ->
                    io:format("[ERROR] Failed to send a message ~p to User: ~p, Error: ~p~n", [Sender, User, Error]),
                    {error, "Failed to send message"}
            end
    end.


get_friends_online(Username) ->
    case user_storage:find_user_friends(Username) of
        {ok, Friends} ->
            FriendsList = string:tokens(binary_to_list(Friends), "\n"),
            OnlineUsers = get_online_users(),
	    OnlineUsersStrings = [binary_to_list(User) || User <- OnlineUsers],
            FriendsOnline = [Friend || Friend <- FriendsList, lists:member(Friend, OnlineUsersStrings)],
            {ok, FriendsOnline};
        {error, Reason} ->
            {error, Reason}
    end.

get_online_users() ->
    [Username || {Username, _Pid} <- ets:tab2list(user_sessions)].

