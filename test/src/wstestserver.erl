-module(wstestserver).

-behaviour(application).


%% API 
-export([start/0]).


%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% API
%% ===================================================================

start() ->
    application:start(cowboy),
    application:start(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  
    Dispatch = [
		{'_', [
		       {[<<"websocket">>], websocket_handler, []}
		      ]}
	       ],
    cowboy:start_listener(my_http_listener, 100,
			  cowboy_tcp_transport, [{port, 8080}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]
			 ),
    wstestserver_sup:start_link().

stop(_State) ->
    ok.
