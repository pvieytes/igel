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
%% @doc
%%
%% @end
%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%-------------------------------------------------------------------

-module(igel_client).

-include_lib("wsock/include/wsock.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1
	]).


%% Debug
-compile([export_all]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% State

%% Client Status
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSE,2).

-record(callbacks, {
	  on_open=fun() ->
			  default_on_open()
		  end,	 
	  on_msg=fun(Msg) -> 
			 default_on_msg(Msg) 
		 end,
	  on_error=fun() ->
			   default_on_error()
		   end,
	  on_close=fun() ->
			   default_on_close()
		   end
	 }).

-record(state, {
	  openhandshake,
	  handshakeresponse,
	  socket,
	  status=?CLOSE,
	  callbacks=#callbacks{},
	  connection_resp_to
	 }).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

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
init(_Params) ->
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
handle_call({connect, Url}, From, State) ->
    case State#state.status of
	?CLOSE ->
	    case parse_ws_url(Url) of
		error -> 
		    {error, "url is not valid"};
		{Host, Port, Path} ->  
		    case gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,true}]) of
			{ok, Sock} ->
			    {ok, HandshakeRequest} = wsock_handshake:open(Path, Host, Port),
			    Req = wsock_http:encode(HandshakeRequest#handshake.message),
			    ok = gen_tcp:send(Sock, Req),
			    inet:setopts(Sock, [{packet, http}]),
			    CallBacks = State#state.callbacks,
			    OnOpen = CallBacks#callbacks.on_open,
			    OnOpen(),			   		    
			    {noreply, State#state{socket=Sock, 
						  openhandshake=HandshakeRequest,
						  status=?CONNECTING,
						  connection_resp_to=From}};
			TcpError ->
			    {reply, {error, TcpError}, State}
		    end
	    end;
	?CONNECTING ->
	    {reply,
	     {error, "client is connecting, please wait"},
	     State};
	?OPEN ->
	    {reply,
	     {error, "client is already connect"},
	     State}
    end;

handle_call(disconnect, _From, State) ->
    case  State#state.status of
	?CLOSE ->
	    R = {error, "client is not connected"},
	    {reply, R, State};
	_status ->
	    Socket = State#state.socket,
	    gen_tcp:close(Socket),
	    CallBacks = State#state.callbacks,
	    OnClose = CallBacks#callbacks.on_close,
	    OnClose(),
	    {reply, ok, State#state{status=?CLOSE}}
    end;    

handle_call({send, Data}, _From, State) ->
    R = 
	case State#state.status of
	    ?OPEN ->
		Message = wsock_message:encode(Data, [mask, text]),
		case gen_tcp:send(State#state.socket, Message) of
		    ok ->
			ok;
		    {error, _Reason} = E ->
			E
		end;
	    _ ->
		{error, "ws is not connected"}
	end,
    {reply, R, State};

handle_call({override_callback, CallbackInfo}, _From, State) ->
    {Error, NewState} = 
	case CallbackInfo of
	    CallbackInfo when is_list(CallbackInfo) ->
		lists:foldl(
		  fun({CbKey, CallBack}, {ErrorAcc, StateAcc}) ->
			  case override_callback(CbKey, CallBack, StateAcc) of
			      error -> 
				  E = ErrorAcc ++ 
				      lists:flatten(io_lib:format("~s is not a callback; ", [CbKey])),
				  {E, StateAcc};
			      NState ->
				  {ErrorAcc, NState}
			  end
		  end,
		  {"", State},
		  CallbackInfo);
	    {CbKey, CallBack} ->
		case override_callback(CbKey, CallBack, State) of
		    error -> 
			{lists:flatten(io_lib:format("~s is not a callback; ", [CbKey])),
			 State};
		    NState ->
			{"", NState}
		end
	end,
    case Error of
	"" ->
	    {reply, ok, NewState};
	Error ->
	    {reply, {error, Error}, NewState}
    end;
	
handle_call(close, _From, State) ->
    {stop, ignore, ok, State};

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


%% MANAGE SOCKET MESSAGES

%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,_Msg}}, State) ->
    HttpMsg = #http_message{type=response, 
			     start_line=[{status, "101"}],
			     headers=[]},
    State1 = State#state{status=?CONNECTING,
			 socket=Socket,
			handshakeresponse=HttpMsg},
    {noreply, State1};

%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}},State) ->
    case State#state.status of
        ?CONNECTING ->
	    Key = case Name of 
		      Name when is_atom(Name) ->atom_to_list(Name);
		      Name when is_list(Name) -> Name
		  end,
	    Response = State#state.handshakeresponse,
	    Headers = Response#http_message.headers,
            NewState = State#state{
			 socket=Socket,
			 handshakeresponse=
			     Response#http_message{headers=[{Key, Value}|Headers]}
			},
            {noreply, NewState};
        undefined ->
            %% Bad state should have received response first
    	    ConnectionFrom = State#state.connection_resp_to,
	    gen_server:reply(ConnectionFrom, {error, "connection error"}),
            {stop, error, State}
    end;

%% Once we have all the headers, check for the 'Upgrade' flag 
handle_info({http, Socket, http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
    OpenHandshake = State#state.openhandshake,
    HandshakeResponse = State#state.handshakeresponse,
    ConnectionFrom = State#state.connection_resp_to,
    case wsock_handshake:handle_response(HandshakeResponse, OpenHandshake) of

	{ok, _} ->
	    inet:setopts(Socket, [{packet, raw}]),
	    gen_server:reply(ConnectionFrom, ok),
	    {noreply, State#state{status=?OPEN}};
	_E ->
	    CallBacks = State#state.callbacks,
	    OnError = CallBacks#callbacks.on_error,
	    OnError(),
	    gen_server:reply(ConnectionFrom, {error, "connection error"}),
	    {noreply, State#state{status=?CLOSE}}  
    end;

%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data},State) ->
    case State#state.status of
        ?OPEN ->
	    case  wsock_message:decode(Data, []) of
		[Msg=#message{type=text}|_] ->
		    CallBacks = State#state.callbacks,
		    OnMsg = CallBacks#callbacks.on_msg,
		    OnMsg(Msg#message.payload);
		_Else ->
		    CallBacks = State#state.callbacks,
		    OnError = CallBacks#callbacks.on_error,
		    OnError()
	    end,
	    {noreply, State};
	_ ->
	    {noreply, State#state{status=?CLOSE}}
    end;

handle_info(_Info, State) ->
    io:format("dbg handle info received: ~p~n", [_Info]),
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
%%% default callbacks
%%%===================================================================

default_on_open()->
    io:format("default on_open.~n").

default_on_msg(Msg) ->
    io:format("default on_msg; receive: ~p~n", [Msg]).

default_on_error()->
    io:format("default on_error.~n").

default_on_close() ->
    io:format("default on_close.~n").




%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% override a callback
%%
%% @spec override_callback(CbKey::atom(), CallBack::fun(), State::#state) ->
%%  NewState::#state | error
%% @end
%%--------------------------------------------------------------------
override_callback(CbKey, Callback, State) ->
    Index = 
	case CbKey of
	    on_open -> #callbacks.on_open;
	    on_msg -> #callbacks.on_msg;
	    on_error -> #callbacks.on_error;
	    on_close -> #callbacks.on_close;
	    _ -> error
	end,
    case Index of
	error -> error;
	Index ->
	    Callbacks = State#state.callbacks,
	    NewCallBacks = erlang:setelement(Index, Callbacks, Callback),
	     State#state{callbacks=NewCallBacks}
    end.	       





%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the ws url string to a tuple
%%
%% @spec parse_ws_url(WsUrl::strin()) -> {Domain::string(), Port::integer(), Path::string()} 
%% @end
%%--------------------------------------------------------------------
parse_ws_url(WsUrl) ->
    Url =
    	case string:str(WsUrl, "ws://") of
    	    1 ->
    		string:substr(WsUrl, 6);
    	    0 ->
    		WsUrl
    	end,
    {Domain, Port, PathList} =
	case string:tokens(Url, ":") of
	    [Url] ->
		case string:tokens(Url, "/") of
		    [Dom] ->
			{Dom, 80, []};
		    [Dom|Rest] ->
			{Dom, 80, Rest}
	       	end;
	    [Dom, PortRest] ->
		case string:tokens(PortRest, "/") of
		    [PortStr|Rest] ->
			{P, _} = string:to_integer(PortStr),
			{Dom, P, Rest}
		end
	end,
    Path = 
	case PathList of
	    [] ->
		"/";
	    PathList ->
		"/" ++ string:join(PathList, "/")
	end,
    {Domain, Port, Path}.




