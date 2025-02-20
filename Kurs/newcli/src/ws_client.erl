-module(ws_client).

-export([connect/3, send_message/3, create_room/3, join_room/3, send_to_room/4]).

connect(Host, Port, Token) ->
    {ok, ConnPid} = gun:open(Host, Port),
    gun:await_up(ConnPid),
    Path = <<"/ws?token=", Token/binary>>,
    WSRef = gun:ws_upgrade(ConnPid,Path),
    receive
        {gun_upgrade, ConnPid, WSRef, _, _} ->
            io:format("WebSocket соединение установлено~n"),
	    {ok, ConnPid, WSRef};
        {gun_response, ConnPid, _, Status, _} ->
            io:format("Не удалось установить WebSocket соединение. Статус: ~p~n", [Status]),
            {error, Status}
    after 5000 ->
        io:format("Истекло время ожидания ответа от WebSocket~n"),
        timeout
    end.

%% Отправка произвольного сообщения
send_message(ConnPid, WSRef, Message) ->
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Сообщение отправлено: ~s~n", [Message]).

%% Создание комнаты
create_room(ConnPid, WSRef, RoomName) ->
    Message = <<"create_room ", RoomName/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Запрос на создание комнаты отправлен: ~s~n", [RoomName]).

%% Подключение к комнате
join_room(ConnPid, WSRef, RoomName) ->
    Message = <<"join_room ", RoomName/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Запрос на подключение к комнате отправлен: ~s~n", [RoomName]).

%% Отправка сообщения в комнату
send_to_room(ConnPid, WSRef, RoomName, Message) ->
    Msg = <<"send_message ", RoomName/binary, " ", Message/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Msg}),
    io:format("Сообщение в комнату ~s отправлено: ~s~n", [RoomName, Message]).
