-module(multi_test).

-include_lib("eunit/include/eunit.hrl").

parse_request_test() ->
	?assertEqual(get, serv2:parse_request(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
	?assertEqual(post, serv2:parse_request(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n">>)),
	?assertEqual(delete, serv2:parse_request(<<"DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
	?assertEqual(unknown, serv2:parse_request(<<"PUT / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)).

parse_accept_test() ->
	?assertEqual(html, serv2:parse_accept(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html\r\n\r\n">>)),
        ?assertEqual(json, serv2:parse_accept(<<"DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
        ?assertEqual(unknown, serv2:parse_accept(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/plain\r\n\r\n">>)).

parse_content_test() ->
	?assertEqual(html, serv2:parse_content(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n">>)),
	?assertEqual(json, serv2:parse_content(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n">>)),
	?assertEqual(unknown, serv2:parse_content(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/plain\r\n\r\n">>)).

method_accept_test() ->
	?assertMatch({post, _, html}, serv2:method_accept(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n">>)),
	?assertMatch({post, _, json}, serv2:method_accept(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: application/json\r\n\r\n">>)),
	?assertMatch({get, json, _}, serv2:method_accept(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
	?assertMatch({get,html, _}, serv2:method_accept(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html\r\n\r\n">>)),
	?assertMatch({delete, json, _}, serv2:method_accept(<<"DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
	?assertMatch({delete, html, _}, serv2:method_accept(<<"DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: text/html\r\n\r\n">>)),
	?assertMatch(_, serv2:method_accept(<<"PUT / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/plain\r\n\r\n">>)).

process_request_test()->
	Res = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 25\r\n\r\n{\"message\": \"GET response\"}",
	?assertEqual(Res, serv2:process_request(<<"GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)),
	Res2 = "HTTP/1.1 201 Created\r\nContent-Type: text/html\r\nContent-Length: 12\r\n\r\n<html><h1>POST</h1></html>",
	?assertEqual(Res2, serv2:process_request(<<"POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n">>)),
	Res3 = "HTTP/1.1 204 No content\r\nContent-Type: application/json\r\nContent-Length: 0\r\n\r\n",
	?assertEqual(Res3, serv2:process_request(<<"DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n">>)).

start_test() ->
	{ok,LSock} = gen_tcp:listen(8080,[binary, {active, false},{packet, 0},{reuseaddr, true}]),
	?assertMatch({ok, _} , {ok,LSock}),
	gen_tcp:close(LSock).

accept_loop_test() ->
	{ok,LSock} = gen_tcp:listen(8080,[binary, {active, false},{packet, 0},{reuseaddr, true}]),
	Pid = spawn(fun() -> serv2:accept_loop(LSock) end),
	?assertNotEqual(undefined, Pid),
	gen_tcp:close(LSock).

handle_client_test() ->
	{ok,LSock} = gen_tcp:listen(8080,[binary, {active, false},{packet, 0},{reuseaddr, true}]),
	Pid = spawn(fun() -> serv2:handle_client(LSock) end),
	{ok,Sock} = gen_tcp:connect("localhost", 8080,[binary, {active, false}]),
	?assertNotMatch(undefined, Pid),
	gen_tcp:close(LSock),
	gen_tcp:close(Sock).

create_requset_test() ->
	?assertEqual("POST / HTTP/1.1\r\nHost: localhost:8080\r\nContent-Type: text/html\r\n\r\n", cli:create_request(post)),
	?assertEqual("DELETE / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n", cli:create_request(delete)),
	?assertEqual( "GET / HTTP/1.1\r\nHost: localhost:8080\r\nAccept: application/json\r\n\r\n", cli:create_request(get)).
