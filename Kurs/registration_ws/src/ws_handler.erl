-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/3, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    TokenParam = proplists:get_value(<<"token">>, Qs),

    case TokenParam of
        undefined ->
            Req1 = cowboy_req:reply(401, #{}, <<"Missing or invalid token">>, Req0),
            {shutdown, Req1, State};
        TokenValue ->
            case verify_token(binary_to_list(TokenValue)) of
                {ok, Username} ->
                    io:format("WebSocket connected for user: ~p~n", [Username]),
                    {cowboy_websocket, Req0, #{username => Username}};
                {error, Reason} ->
                    Msg = io_lib:format("Invalid token: ~p", [Reason]),
                    Req1 = cowboy_req:reply(401, #{}, list_to_binary(Msg), Req0),
                    {shutdown, Req1, State}
            end
    end.

websocket_init(Req, _Opts, State) ->
    {ok, Req, State}.

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
                {send_message, RoomName, Message} -> 
                    case user_storage:send_message(RoomName, Username, Message) of
                        {ok, _} ->
                            io:format("Message sent to room ~p by user ~p~n", [RoomName, Username]),
                            {reply, {text, <<"Message sent successfully">>}, State};
                        {error, Reason} ->
                            io:format("Message error for room ~p: ~p~n", [RoomName, Reason]),
                            {reply, {text, <<"Error: Unable to send message">>}, State}
                    end;
                invalid ->
                    {reply, {text, <<"Error: Invalid command">>}, State}
            end;
        false ->
            io:format("Ошибка: недопустимое сообщение от клиента: ~p~n", [Msg]),
            {reply, {text, <<"Ошибка: Сообщение не является допустимой строкой">>}, State}
    end;

websocket_handle(_Data, State) ->
    io:format("Неверный формат сообщения: ~p~n", [_Data]),
    {reply, {text, <<"Ошибка: Неверный формат сообщения">>}, State}.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #{username := Username}) ->
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
        [<<"send_message">>, RoomName, Message] -> {send_message, RoomName, Message};
        _ -> invalid
    end.
