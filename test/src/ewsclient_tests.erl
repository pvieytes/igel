%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------
-module(ewsclient_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start/0]).

start() ->
    eunit:test(ewsclient).

echo_websocket_org_test() ->
    %%start ewsclient app
    app_test_funs(),

    %% ws client test functions
    ws_test_funs("ws://echo.websocket.org"),

    %% start local websocket sever
    wstestserver:start(),

    %% ws client test functions
    ws_test_funs("ws://localhost:8080/websocket").


app_test_funs() ->
    %% start client without start the app
    ?assertException(_ClassPattern, _TermPattern, ewsclient:start_client()),
    
    %% start ewsclient app
    ewsclient:start().



ws_test_funs(Host) ->
  
    %% websocket test funs
    ?debugMsg("Host: " ++ Host),
 
    %% start client
    WsStarted =  ewsclient:start_client(),
    ?assertMatch({ok, _Ws}, WsStarted),
    {ok, Ws} = WsStarted,
    ?assertMatch({error, _}, Ws:send("test")),
    
    %% override callbacks
    TestProcessPid = self(),
    FMirror =fun (Msg) -> TestProcessPid !  Msg end,
    ?assertMatch(ok, Ws:override_callback({on_msg, FMirror})),
    FOnclose = fun () ->  TestProcessPid ! closed end,
    ?assertMatch(ok, Ws:override_callback([{on_close, FOnclose}])),
    FOnOpen =  fun () ->  TestProcessPid ! open end,
    ?assertMatch(ok, Ws:override_callback({on_open, FOnOpen})),

    %% connect
    ?assertMatch(ok, Ws:connect(Host)),
    ?assertMatch(open, read_mailbox()),
    ?assertMatch({error,_}, Ws:connect(Host)),
   
    %%send msgs
    Text = "test",
    ?assertMatch(ok, Ws:send(Text)),
    ?assertMatch(Text, read_mailbox()),

    %% disconnect
    ?assertMatch(ok, Ws:disconnect()),
    ?assertMatch(closed, read_mailbox()),
    ?assertMatch({error,_}, Ws:disconnect()),

    %% start client with params
    Parmas = [{connect, Host},
    	      {callbacks, 
    	       [
    		{on_open, FOnOpen},
    		{on_close, FOnclose},
    		{on_msg, FMirror}
    	       ]}
    	     ],
    Ws2Started = ewsclient:start_client(Parmas),
    ?assertMatch({ok, _Ws2}, Ws2Started),
    ?assertMatch(open, read_mailbox()).
   

read_mailbox()->
    receive
	R ->
	    R
    after 5000 ->
	    timeout
    end.
