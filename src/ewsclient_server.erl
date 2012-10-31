%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------

-module(ewsclient_server).

-include_lib("ewsclient/include/ewsclient.hrl").

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
%% Ready States
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).
-record(callbacks, {on_msg=fun(Msg) -> 
				   default_on_msg(Msg) 
			   end}).
-record(state, {socket,
		status=?CLOSED,
		headers=[],
		callbacks=#callbacks{},
		response_connection
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
handle_call({connect, Url, ResponseTo}, _From, State) ->
    case State#state.status of
	?CLOSED ->
	    R =
		case parse_ws_url(Url) of
		    error -> 
			{error, "url is not valid"};
		    {Host, Port, Path} ->  
			case gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,true}]) of
			    {ok, Sock} ->
				Request = ewsclient_ws13:create_handshake_req(Host, Port, Path),
				ok = gen_tcp:send(Sock,Request),
				inet:setopts(Sock, [{packet, http}]),
				{ok, Sock};
			    TcpError ->
				{error, TcpError}
			end
		end,
	    case R of
		{ok, S} ->
		    {reply, ok, State#state{socket=S, 
					    status=?CONNECTING,
					    response_connection=ResponseTo}};
		Error->
		    {reply, Error, State}
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
	?CLOSED ->
	    R = {error, "client is not connected"},
	    {reply, R, State};
	_status ->
	    Socket = State#state.socket,
	    gen_tcp:close(Socket),
	    {reply, ok, State#state{status=?CLOSED,
				    headers=[]}}
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

handle_call({override_callback, {Type, Fun}}, _From, State) ->
    Callbacks = State#state.callbacks,
    case Type of
	on_msg ->
	    NewCallBacks = Callbacks#callbacks{on_msg=Fun},
	    {reply, ok, State#state{callbacks=NewCallBacks}};
	_ ->
	    {reply, {error, "callback key not valid", State}}
    end;

handle_call(stop, _From, State) ->
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

%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,_Msg}}, State) ->
    State1 = State#state{status=?CONNECTING,socket=Socket},
    {noreply, State1};

%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}},State) ->
    case State#state.status of
        ?CONNECTING ->
            H = [{Name,Value} | State#state.headers],
            State1 = State#state{headers=H,socket=Socket},
            {noreply,State1};
        undefined ->
            %% Bad state should have received response first
            {stop, error, State}
    end;

%% Once we have all the headers, check for the 'Upgrade' flag 
handle_info({http, Socket, http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
    Headers = State#state.headers, 
    case ewsclient_ws13:check_handshake_server_response(Headers) of
	ok ->
	    inet:setopts(Socket, [{packet, raw}]),
	    State#state.response_connection ! {self(), connected},
	    {noreply, State#state{status=?OPEN}};
	_ ->
	    {noreply, State#state{status=?CLOSED}}  
    end;

%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data},State) ->
    case State#state.status of
        ?OPEN ->
	    case ewsclient_ws13:parse_received_data(Data) of
		{?OP_TEXT, String}->
		    CallBacks = State#state.callbacks,
		    OnMsg = CallBacks#callbacks.on_msg,
		    OnMsg(String);
		_Else ->
		    io:format("dbg received: ~p~n", [_Else])
	    end,
	    {noreply, State};
	_ ->
	    {noreply, State#state{status=?CLOSED}}
    end;

handle_info(_Info, State) ->
    io:format("dbg received: ~p~n", [_Info]),
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

default_on_msg(Msg) ->
    io:format("default on_msg :: receive: ~p~n", [Msg]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

