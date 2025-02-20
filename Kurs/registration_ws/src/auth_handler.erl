-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    io:format("Req0:: ~p~n", [Req0]),
    io:format("Req1:: ~p~n", [Req1]),
    io:format("Received body: ~p~n", [Body]),
    Params = jsx:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Params, undefined),
    Password = maps:get(<<"password">>, Params, undefined),

    case {Username, Password} of
        {undefined, _} -> respond(Req1, 400, <<"Missing username">>);
        {_, undefined} -> respond(Req1, 400, <<"Missing password">>);
        _ ->
            case user_storage:authenticate(binary_to_list(Username),(Password)) of
    		true ->
		        Token = base64:encode(<<Username/binary, ":", Password/binary>>),
		        respond(Req1, 200, jsx:encode(#{token => Token})),
			{ok, Req1, State};
		false ->
		        respond(Req1, 401, <<"Invalid credentials">>),
			{ok, Req1, State}
	    end		
    end.

respond(Req, Code, Message) ->
    cowboy_req:reply(Code, #{}, Message, Req).


