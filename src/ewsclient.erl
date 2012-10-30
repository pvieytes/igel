%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------

-module(ewsclient).


%% API
-export([start/0,
	 start_client/0,
	 start_client/1,
	 connect_client/2,
	 send/2]).


-define(CHILD(Id, Params), {Id, {ewsclient_server, start_link, [Params]}, permanent, 5000, worker, dynamic}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start ewsclient application
%%
%% @spec start() -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% start ewsclient application
%%
%% @spec start() -> {ok, pid()} | {error, Error}
%%
%% @end
%%--------------------------------------------------------------------
start_client()->
    start_client([]).
   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start ewsclient application
%%
%% @spec start(Params::[{CallBack::callback(), Fun::fun()}]) -> {ok, pid()} | {error, Error}
%%
%%  callback() = on_open | on_error | on_message | on_closed
%%--------------------------------------------------------------------
start_client(Params)->
    RandomId = now(),
    ChildSpec =  ?CHILD(RandomId, Params),
    supervisor:start_child(ewsclient_sup, ChildSpec).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connect the client
%%
%% @spec connect_client(Client::pid(), Url::string()) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
connect_client(Client, Url) ->
    gen_server:call(Client, {connect, Url}).

send(Client, Data) ->
    gen_server:call(Client, {send, Data}).  
