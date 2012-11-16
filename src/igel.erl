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

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start/0,
	 start_client/0,
	 start_client/1,
	 close_client/1,
	 disconnect/1,
	 connect/2,
	 send/2,
	 override_callback/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(CHILD(Id, Params), {Id, {igel_client, start_link, [Params]}, permanent, 5000, worker, dynamic}).


-define(SERVER, ?MODULE). 

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
    gen_server:call(?MODULE, {start_client, Params}).


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
    gen_server:call(WsClientPid,{connect, Url}).

    %% gen_server:call(?MODULE, {connect, Url, WsClientPid}). 

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({start_client, Params}, _From, State) ->
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
			override_callback(CallbackInfo, {?MODULE, ClientPid})
		end,
	    ConnectStatus =
		case OverrideStatus of
		    ok->		  
			%% connect
			case proplists:get_value(connect, Params) of
			    undefined ->
				ok;
			    Host ->
				connect(Host, {?MODULE, ClientPid})
			end;
		    Else -> Else
		end,
	    
		   
	    case ConnectStatus of
		ok ->
		    {reply, {ok, {?MODULE, ClientPid}}, State};
		Error -> Error
	    end;
    	Error ->
    	  {reply, {error, Error}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
     {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

