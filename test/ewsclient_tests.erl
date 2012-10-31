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
    Host = "echo.websocket.org",
    ?debugMsg("echo.websocket.org tests"),
    ws_test_funs(Host).
    


ws_test_funs(Host) ->
    ?assertException(_ClassPattern, _TermPattern, ewsclient:start_client()),
    ?assertMatch(ok, ewsclient:start()),
    Started =  ewsclient:start_client(),
    ?assertMatch({ok, Ws}, Started),
    {ok, Ws} = Started,
    ?assertMatch({error, _}, Ws:send("test")),
    ?assertMatch(ok, Ws:connect(Host)),
    ?assertMatch({error,_}, Ws:connect(Host)),
    TestProcessPid = self(),
    FOnMsg =fun (Msg) -> TestProcessPid !  Msg end,
    ?assertMatch(ok, Ws:override_callback({on_msg, FOnMsg})),
    Text = "test",
    ?assertMatch(ok, Ws:send(Text)),
    Received =get_mailbox(),
    ?assertMatch(Text, Received),
    ?assertMatch(ok, Ws:disconnect()),
    ?assertMatch({error,_}, Ws:disconnect()).


    


get_mailbox()->
    receive
	R ->
	    R
    after 5000 ->
	    timeout
    end.
