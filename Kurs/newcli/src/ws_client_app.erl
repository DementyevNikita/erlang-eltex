-module(ws_client_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	io:format("Starting ws_client application...~n"),
	application:ensure_all_started(cowlib),
	application:ensure_all_started(gun),
	application:ensure_all_started(jsx),
	ws_client_sup:start_link(),
	{ok, self()}.

stop(_State) ->
    ok.

