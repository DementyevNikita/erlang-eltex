-module(serv2).
-export([start/0, parse_request/1, parse_accept/1, parse_content/1, method_accept/1, process_request/1, accept_loop/1, handle_client/1]).


start()->
	{ok,ListSock} = gen_tcp:listen(8080,[binary, {active, false},{packet, 0},{reuseaddr, true}]),
	io:format("HTTP включен на порту 8080 ~n"),
	spawn(fun() -> accept_loop(ListSock)end).

accept_loop(ListSock) ->
	{ok, Sock} = gen_tcp:accept(ListSock),
	spawn(fun() -> handle_client(Sock) end),
	accept_loop(ListSock).

handle_client(Sock) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, Data} ->
			io:format("Received ответ: ~p~n", [Data]),
			Res = process_request(Data),
			gen_tcp:send(Sock, Res),
			handle_client(Sock);
		{error, closed} ->
			io:format("Closed connection ~n")
	end.

process_request(Data) ->
	case method_accept(Data) of 
		{get, json, _} ->
			"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 27\r\n\r\n{\"message\":\"GET response\"}";
		{get, html, _} ->
                        "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 11\r\n\r\n<html><h1>GET</h1></html>";
		{get, _, _} ->
                        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 27\r\n\r\n{\"message\": \"GET response\"}";
		{post, _, html} ->
			"HTTP/1.1 201 Created\r\nContent-Type: text/html\r\nContent-Length: 12\r\n\r\n<html><h1>POST</h1></html>";
		{post, _, json} ->
                        "HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: 28\r\n\r\n{\"message\": \"POST response\"}";
		{post, _, _} ->
                        "HTTP/1.1 201 Created\r\nContent-Type: text/html\r\nContent-Length: 12\r\n\r\n<html><h1>POST</h1></html>";
		{delete, json, _} ->
			"HTTP/1.1 204 No content\r\nContent-Type: application/json\r\nContent-Length: 0\r\n\r\n";
		{delete, html, _} ->
                        "HTTP/1.1 204 No content\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n";
		{delete, _, _} ->
                        "HTTP/1.1 204 No content\r\nContent-Type: application/json\r\nContent-Length: 0\r\n\r\n";
		{_,_,_} ->
			"HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\nContent-Length: 15\r\n\r\nBad Request"
	end.


method_accept(Data) ->
	Method = parse_request(Data),
	Acce = parse_accept(Data),
	Cont = parse_content(Data),
	{Method, Acce, Cont}.

parse_request(Data) ->
	case binary:split(Data, <<" ">>) of
		[<<"GET">>, _] -> get;
		[<<"POST">>, _] -> post;
		[<<"DELETE">>, _] -> delete;
		_ -> unknown
	end.

parse_accept(Data) ->
        case binary:split(Data, <<"Accept: ">>) of
		[_, Accept] -> 
			case binary:split(Accept, <<"\r\n\r\n">>) of
				[<<"application/json">>, _] -> json;
				[<<"text/html">>, _] -> html;
				_ -> unknown
			end;
		_ -> unknown
        end.

parse_content(Data) ->
        case binary:split(Data, <<"Content-Type: ">>) of
                [_, ContentType] ->
                        case binary:split(ContentType, <<"\r\n\r\n">>) of
                                [<<"application/json">>, _] -> json;
                                [<<"text/html">>, _] -> html;
                                _ -> unknown
                        end;
                _ -> unknown
        end.


