-module(serv2).
-behaviour(gen_server).
-export([start_link/0, parse_request/1, parse_accept/1, parse_content/1, method_accept/1, process_request/1, init/1, handle_client/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, parse_status_code/1]).

-record(request_log, {ip, resource, method, status_code}).

start_link() ->
	  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])->
	mnesia:create_schema([node()]),
	mnesia:start(),
	create_request_log_table(),
	{ok,ListSock} = gen_tcp:listen(8080,[binary, {active, false},{reuseaddr, true}]),
	io:format("HTTP включен на порту 8080 ~n"),
	spawn(fun() -> accept_loop(ListSock)end),
	{ok,ListSock}.

accept_loop(ListSock) ->
	{ok, Sock} = gen_tcp:accept(ListSock),
	gen_server:cast(?MODULE, {client_connected, Sock}),
	accept_loop(ListSock).

handle_cast({client_connected, Sock}, State) ->
	spawn(fun() -> handle_client(Sock) end),
	{noreply, State}.

handle_client(Sock) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, Data} ->
			io:format("Received ответ: ~p~n", [Data]),
			Res = process_request(Data),
			{Ip, Resource, Method, StatusCode} = parse_request_log(Data, Res),
			log_request({Ip, Resource, Method, StatusCode}),
			gen_tcp:send(Sock, Res),
			handle_client(Sock);
		{error, closed} ->
			io:format("Closed connection ~n")
	end.

handle_call(_Request, _From, State) ->
	{reply, ok , State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

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

log_request({Ip, Resource, Method, StatusCode}) ->
	Fun = fun() -> 
		Record = #request_log{
			    ip = Ip,
			    resource = Resource,
			    method = Method,
			    status_code = StatusCode
			   },
		mnesia:write(Record)
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} ->
			io:format("Информация о запросе успешно сохранена.~n");
		{aborted, Reason} ->
			io:format("Ошибка сохранения в базу данных: ~p~n", [Reason])
	end.

parse_request_log(Data, Res) ->
	Ip = "127.0.0.1",
	Resource = parse_resourse(Data),
	Method = parse_request(Data),
	StatusCode = parse_status_code(Res),
	{Ip, Resource, Method, StatusCode}.

parse_resourse(Data) ->
	[_, Resourse1 | _] = binary:split(Data,<<"Host: ">>),
	[Resourse2, _] =  binary:split(Resourse1,<<"\r\n">>),
	case binary:split(Resourse2,<<":">>) of
                [Resourse3 ,  _] ->
                        Resourse = binary_to_list(Resourse3),
			Resourse;
                _ -> 500
        end.


parse_status_code(Res) ->
	[_, Res1 | _] = string:split(Res," "),
	case string:split(Res1,"\r\n") of
		[StatusCode ,  _] ->
			StatusCode;
		_ -> 500
	end.

create_request_log_table() ->
	case mnesia:create_table(request_log, [{attributes, record_info(fields, request_log)}, {disc_copies, [node()]}, {type, bag}]) of
		{atomic, ok} ->
			io:format("Таблица request_log создана успешно.~n");
		{aborted, {already_exists, request_log}} ->
			io:format("Таблица request_log уже существует.~n");
		_ ->
			io:format("Ошибка при создании таблицы request_log.~n")
	end.
