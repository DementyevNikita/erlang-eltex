-module(cli).
-export([start/1, create_request/1]).

start(RequestType) ->
	{ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
	Request = create_request(RequestType), 
	gen_tcp:send(Sock,Request), 
	case gen_tcp:recv(Sock, 0) of
                {ok, Data} ->
                        io:format("Received дошло: ~p~n", [Data]),
                        dis_cli_info(Sock);
                {error, Reason} ->
                        io:format("Error: ~p~n", [Reason])
        end,
        gen_tcp:close(Sock).


create_request(get) ->
	"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n";

create_request(geth) ->
        "GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html\r\n\r\n";

create_request(post) ->
	"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n";

create_request(postj) ->
        "POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n";

create_request(delete) ->
        "DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n";

create_request(deleteh) ->
        "DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html\r\n\r\n".

dis_cli_info(Sock) ->
	case inet:sockname(Sock) of 
		{ok,{Ip, Port}} ->
			io:format("Client add: ~p, Client port: ~p~n",[Ip,Port]);
		{error, Reason} ->
                        io:format("Error111: ~p~n", [Reason])
	end.

