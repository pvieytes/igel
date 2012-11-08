%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------

-module(igel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    igel_sup:start_link().

stop(_State) ->
    ok.
