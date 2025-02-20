-module(http_client).

-export([register_user/2]).

register_user(Username, Password) ->
    Host = "localhost",
    Port = 8080,
    URL = io_lib:format("http://~s:~p/register", [Host, Port]),
    UserData = [{username, Username}, {password, Password}],
    JSONBody = jsx:encode(UserData),
    case httpc:request(post, {URL, [{"Content-Type", "application/json"}], "application/json", JSONBody}, [], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            io:format("Регистрация успешна: ~s~n", [ResponseBody]);
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("Ошибка регистрации: Код - ~p Ответ - ~s~n", [StatusCode, ResponseBody]);
        {error, Reason} ->
            io:format("Ошибка подключения: ~p~n", [Reason])
    end.

