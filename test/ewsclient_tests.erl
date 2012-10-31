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
    ?debugMsg("echo.websocket.org tests"),
    ?assertMatch(true, true).

