%%-------------------------------------------------------------------
%% @author Pablo Vieytes <mail@pablovieytes.com>
%% @copyright (C) 2012, Pablo Vieytes
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%
%% @end
%% Created : 31 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%-------------------------------------------------------------------
-module(igel_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start/0]).

start() ->
    eunit:test(igel).

echo_websocket_org_test() ->
    %%start igel app
    app_test_funs(),

    %% ws client test functions
    ws_test_funs("ws://echo.websocket.org"),

    %% start local websocket sever
    wstestserver:start(),

    %% ws client test functions
    ws_test_funs("ws://localhost:8080/websocket").


app_test_funs() ->
    %% start client without start the app
    ?assertException(_ClassPattern, _TermPattern, igel:start_client()),
    
    %% start igel app
    igel:start().



ws_test_funs(Host) ->
  
    %% websocket test funs
    ?debugMsg("Host: " ++ Host),
 
    %% start client
    WsStarted =  igel:start_client(),
    ?assertMatch({ok, _Ws}, WsStarted),
    {ok, Ws} = WsStarted,
    ?assertMatch({error, _}, igel:send(Ws, "test")),
    
    %% override callbacks
    TestProcessPid = self(),
    FMirror =fun (Msg) -> TestProcessPid !  Msg end,
    ?assertMatch(ok, igel:override_callback(Ws, {on_msg, FMirror})),
    FOnclose = fun () ->  TestProcessPid ! closed end,
    ?assertMatch(ok, igel:override_callback(Ws, [{on_close, FOnclose}])),
    FOnOpen =  fun () ->  TestProcessPid ! open end,
    ?assertMatch(ok, igel:override_callback(Ws, {on_open, FOnOpen})),

    %% connect
    ?assertMatch(ok, igel:connect(Ws, Host)),
    ?assertMatch(open, read_mailbox()),
    ?assertMatch({error,_}, igel:connect(Ws, Host)),
   
    %%send msgs
    Text = "test",
    ?assertMatch(ok, igel:send(Ws, Text)),
    ?assertMatch(Text, read_mailbox()),

    %% disconnect
    ?assertMatch(ok, igel:disconnect(Ws)),
    ?assertMatch(closed, read_mailbox()),
    ?assertMatch({error,_}, igel:disconnect(Ws)),

    %% start client with params
    Parmas = [{connect, Host},
    	      {callbacks, 
    	       [
    		{on_open, FOnOpen},
    		{on_close, FOnclose},
    		{on_msg, FMirror}
    	       ]}
    	     ],
    Ws2Started = igel:start_client(Parmas),
    ?assertMatch({ok, _Ws2}, Ws2Started),
    ?assertMatch(open, read_mailbox()).
   

read_mailbox()->
    receive
	R ->
	    R
    after 5000 ->
	    timeout
    end.
