-module(ws_client).

-export([connect/3, send_message/3, create_room/3, join_room/3, send_to_room/4,startpingloop/2]).

connect(Host, Port, Token) ->
    {ok, ConnPid} = gun:open(Host, Port),
    gun:await_up(ConnPid),
    Path = <<"/ws?token=", Token/binary>>,
    WSRef = gun:ws_upgrade(ConnPid,Path),
    receive
        {gun_upgrade, ConnPid, WSRef, _, _} ->
            io:format("WebSocket соединение установлено~n"),
	    spawn(?MODULE, startpingloop, [ConnPid, WSRef]),
	    {ok, ConnPid, WSRef};
        {gun_response, ConnPid, _, Status, _} ->
            io:format("Не удалось установить WebSocket соединение. Статус: ~p~n", [Status]),
            {error, Status}
    after 5000 ->
        io:format("Истекло время ожидания ответа от WebSocket~n"),
        timeout
    end.

send_message(ConnPid, WSRef, Msg) ->
    Message = <<"custom_message ", Msg/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Сообщение отправлено: ~s~n", [Message]),
    wait_for_reply().

create_room(ConnPid, WSRef, RoomName) ->
    Message = <<"create_room ", RoomName/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Запрос на создание комнаты отправлен: ~s~n", [RoomName]),
    wait_for_reply().

join_room(ConnPid, WSRef, RoomName) ->
    Message = <<"join_room ", RoomName/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Message}),
    io:format("Запрос на подключение к комнате отправлен: ~s~n", [RoomName]),
    wait_for_reply().

send_to_room(ConnPid, WSRef, RoomName, Message) ->
    Msg = <<"send_message ", RoomName/binary, " ", Message/binary>>,
    gun:ws_send(ConnPid, WSRef, {text, Msg}),
    io:format("Сообщение в комнату ~s отправлено: ~s~n", [RoomName, Message]),
    wait_for_reply().

startpingloop(ConnPid, WSRef) ->
    sendping(ConnPid, WSRef),
    timer:sleep(10000),
    case is_process_alive(ConnPid) of
        true ->
            startpingloop(ConnPid, WSRef);
        false ->
            io:format("WebSocket соединение завершено~n"),
            exit(self(), normal)
    end.

sendping(ConnPid, WSRef) ->
    Message = <<"ping">>,
    gun:ws_send(ConnPid, WSRef, {binary, Message}).

wait_for_reply() ->
    receive
        {gun_ws, _ConnPid, _WSRef, {text, Reply}} ->
            io:format("Ответ от сервера: ~s~n", [Reply]),
            ok
        after 5000 ->
            io:format("Ответ от сервера не получен в течение 5 секунд.~n"),
            timeout
    end.
