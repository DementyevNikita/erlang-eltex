-module(auth_client).
-export([authenticate_user/2]).

authenticate_user(Username, Password) ->
    Host = "localhost",
    Port = 8080,
    URL = io_lib:format("http://~s:~p/auth", [Host, Port]),
    UserData = [{username,Username}, {password,Password}],
    JSONBody = jsx:encode(UserData),
    case httpc:request(post, {URL, [{"Content-Type", "application/json"}], "application/json", JSONBody}, [], []) of
        {ok, {{_, 200, _},_,  ResponseBody}} ->
            io:format("Аутентификация успешна: ~s~n", [ResponseBody]);
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("Ошибка аутентификации: Код ~p Ответ ~s~n", [StatusCode, ResponseBody]);
        {error, Reason} ->
            io:format("Ошибка подключения: ~p~n", [Reason])
    end.

