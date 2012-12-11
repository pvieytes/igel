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
%%
%% @end
%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%-------------------------------------------------------------------

-module(igel).

%% API
-export([start/0,
	start_client/0,
	start_client/1,
	close_client/1,
	disconnect/1,
	connect/2,
	send/2,
	override_callback/2]).

-define(CHILD(Id, Params), {Id, {igel_client, start_link, [Params]}, permanent, 5000, worker, dynamic}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start igel application
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
%% start igel client
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
%% start igel client
%%
%% @spec start_client(List::[Element::element()]) -> {ok, pid()} | {error, Error}
%%
%% element() == {connect , Url::string()} | {callbacks, [{callback(), Fun::fun()}]}
%% callback() = on_open | on_error | on_message | on_close
%%--------------------------------------------------------------------
start_client(Params) ->
    RandomId = make_ref(),
    ChildSpec =  ?CHILD(RandomId, []),
    case supervisor:start_child(igel_sup, ChildSpec) of
	{ok, ClientPid}->
	    %% override funs
	    OverrideStatus =
		case proplists:get_value(callbacks, Params) of
		    undefined ->
			ok;
		    CallbackInfo  ->
			override_callback(ClientPid, CallbackInfo)
		end,
	    ConnectStatus =
		case OverrideStatus of
		    ok->		  
			%% connect
			case proplists:get_value(connect, Params) of
			    undefined ->
				ok;
			    Host ->
				connect(ClientPid, Host)
			end;
		    Else -> Else
		end,


	    case ConnectStatus of
		ok ->
		    {ok, ClientPid};
		Error -> Error
	    end;
    	Error ->
	    {error, Error}
    end.

    %% gen_server:call(?MODULE, {start_client, Params}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Close the client
%%
%% @spec close_client(WsClienPidt::pid()) -> ok
%%
%%--------------------------------------------------------------------
close_client(WsClientPid)->
    gen_server:call(WsClientPid, close).  


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connect the client
%%
%% @spec connect(Url::string(), WsClienPidt::pid()) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
connect(WsClientPid, Url) ->
    gen_server:call(WsClientPid,{connect, Url}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Disconnect the client
%%
%% @spec disconnect(WsClienPidt::pid()) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
disconnect(WsClientPid) ->
    gen_server:call(WsClientPid, disconnect).  

%%--------------------------------------------------------------------
%% @private
%% @doc
%% send data
%%
%% @spec send(Dataa::string, WsClienPidt::pid()) -> ok | {error, Error}
%%
%%--------------------------------------------------------------------
send(WsClientPid, Data) ->
    gen_server:call(WsClientPid, {send, Data}).  


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Override the callback
%%
%% @spec override_callback(CallbackInfo::callbackinfo(),  WsClienPidt::pid()) -> 
%%               ok | {error, Error}
%%
%% callbackinfo() = {CbKey::atom(), Fun::fun()} | [{CbKey::atom(), Fun::fun()}]
%% callback() = on_open | on_error | on_message | on_close
%%--------------------------------------------------------------------
override_callback(WsClientPid, CallbackInfo) ->
    gen_server:call(WsClientPid, {override_callback, CallbackInfo}).
