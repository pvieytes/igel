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
	 close_client/1,
	 disconnect/1,
	 connect/2,
	 send/2,
	 override_callback/2]).


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
%% start ewsclient client
%%
%% @spec start_client() -> {ok, pid()} | {error, Error}
%%
%% @end
%%--------------------------------------------------------------------
start_client()->
    start_client([]).
   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start ewsclient client
%%
%% @spec start_client(List::[Element::element()]) -> {ok, pid()} | {error, Error}
%%
%% element() == {connect , Url::string()} | {callbacks, [{callback(), Fun::fun()}]}
%% callback() = on_open | on_error | on_message | on_close
%%--------------------------------------------------------------------
start_client(Params)->
    RandomId = now(),
    ChildSpec =  ?CHILD(RandomId, Params),
    case supervisor:start_child(ewsclient_sup, ChildSpec) of
	{ok, Pid}->
	     {ok, {?MODULE, Pid}};
	Error ->
	    {error, Error}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Close the client
%%
%% @spec close_client({Module::atom(), WsClienPidt::pid()}) -> ok
%%
%%--------------------------------------------------------------------
close_client({_Mod, WsClientPid})->
    gen_server:call(WsClientPid, close).  


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connect the client
%%
%% @spec connect(Url::string(), {Module::atom(), WsClienPidt::pid()}) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
connect(Url, {_Mod, WsClientPid}) ->
    ResponseTo = self(),
    case gen_server:call(WsClientPid, {connect, Url, ResponseTo}) of
	ok ->
	    receive
		{WsClientPid, connected} ->
		    ok
	    after 5000 ->
		    {error, "time out"}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Disconnect the client
%%
%% @spec disconnect({Module::atom(), WsClienPidt::pid()}) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
disconnect({_Mod, WsClientPid}) ->
    gen_server:call(WsClientPid, disconnect).  


%%--------------------------------------------------------------------
%% @private
%% @doc
%% send data
%%
%% @spec send(Dataa::string, {Module::atom(), WsClienPidt::pid()}) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
send(Data, {_Mod, WsClientPid}) ->
    gen_server:call(WsClientPid, {send, Data}).  


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Override the callback
%%
%% @spec override_callback(CallbackInfo::callbackinfo(), {Module::atom(), WsClienPidt::pid()}) -> 
%%               ok | {error, Error}
%%
%% callbackinfo() = {CbKey::atom(), Fun::fun()} | [{CbKey::atom(), Fun::fun()}]
%% callback() = on_open | on_error | on_message | on_close
%%--------------------------------------------------------------------
override_callback(CallbackInfo, {_Mod, WsClientPid}) ->
    gen_server:call(WsClientPid, {override_callback, CallbackInfo}).
