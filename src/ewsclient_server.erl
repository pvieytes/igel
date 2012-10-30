%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------

-module(ewsclient_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Ready States

-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).
-record(state, {socket,status=?CLOSED,headers=[],callback}).


% macros
-define(OP_CONT, 0).
-define(OP_TEXT, 1).
-define(OP_BIN, 2).
-define(OP_CLOSE, 8).
-define(OP_PING, 9).
-define(OP_PONG, 10).

-define(FRAGMENT_SIZE, 4096).


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
handle_call({connect, Url}, _From, State) ->
    R =
	case parse_ws_url(Url) of
	    error -> 
		{error, "url is not valid"};
	    {Host, Port, Path} ->  
		case gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,true}]) of
		    {ok, Sock} ->
			Req = create_handshake_req(Host,Port, Path),
			io:format("dbg req: ~p~n", [Req]),
			ok = gen_tcp:send(Sock,Req),
			inet:setopts(Sock, [{packet, http}]),
			{ok, Sock};
		    _Error ->
			{error,"Host does not response"}
		end
	end,
    case R of
	{ok, S} ->
	    {reply, ok, State#state{socket=S, status=?CONNECTING}};
	Error->
	    {reply, Error, State}
    end;

handle_call({send, Data}, _From, State) ->
    io:format("dbg socket: ~p~n", [State#state.socket]),
    Message = wsock_message:encode(Data, [mask, text]),
    io:format("dbg msg: ~p~n", [Data]),
    case gen_tcp:send(State#state.socket, Message) of
	ok ->
	    io:format("dbg sent~n"),
	    ok;
	{error, Reason} ->
	    io:format("dbg send error: ~p~n", [Reason])
    end,
    {reply, ok, State};

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
            {stop,error,State}
    end;

%% Once we have all the headers, check for the 'Upgrade' flag 
handle_info({http, Socket, http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
    io:format("dbg status: ~p; socket: ~p~n", [State#state.status, Socket]),
    Headers = State#state.headers, 
    case check_handshake_server_response(Headers) of
	ok ->
	    inet:setopts(Socket, [{packet, raw}]),
	    io:format("dbg connected~n");
	_ ->
	    io:format("dbg *NO* connected~n")
    end,
    {noreply, State};


%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data},State) ->
    case  parse_received_data(Data) of
        {?OP_TEXT, String}->
            io:format("dbg received: ~p~n", [String]);
	_Else ->
	    io:format("dbg received: ~p~n", [_Else])
    end,
    {noreply, State};


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
%%% Internal functions
%%%===================================================================




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the ws url string to a tuple
%%
%% @spec parse_ws_url(WsUrl::strin()) -> {Domain::string(), Port::integer(), Path::string()} | error
%% @end
%%--------------------------------------------------------------------
parse_ws_url(_WsUrl) ->
    %%hardcode example
   %%{"localhost", 8000, "/ws"}.
   {"echo.websocket.org", 80, "/"}.

create_handshake_req(Host, Port, Path)->
    PortStr = io_lib:format("~p", [Port]),
     lists:flatten("GET " ++ Path ++ " HTTP/1.1\r\n" ++ 
    		      "Upgrade: WebSocket\r\n" ++
    		      "Connection: Upgrade\r\n" ++
    		      "Host: " ++ Host ++ ":" ++ PortStr ++ "\r\n" ++
    		      "Origin: " ++ Host ++ ":" ++ PortStr ++ "\r\n" ++
    		      "Sec-WebSocket-Key: vE6RKcwiuiUdNiF1Cpdz8Q==\r\n" ++
    		      "Sec-WebSocket-Version: 13\r\n" ++
    		      "\r\n").


check_handshake_server_response(Headers)->
    Connection = proplists:get_value('Connection', Headers),
    if 
	Connection == "Upgrade" ->
	    Upgrade = proplists:get_value('Upgrade', Headers),
	    if 
		Upgrade == "WebSocket" ->
		    ok;
		true ->
		    error
	    end;
	true ->
	    error
    end.







%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-------+-+-------------+-------------------------------+
%% |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
%% |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
%% |N|V|V|V|       |S|             |   (if payload len==126/127)   |
%% | |1|2|3|       |K|             |                               |
%% +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
%% |     Extended payload length continued, if payload len == 127  |
%% + - - - - - - - - - - - - - - - +-------------------------------+
%% |                               |Masking-key, if MASK set to 1  |
%% +-------------------------------+-------------------------------+
%% | Masking-key (continued)       |          Payload Data         |
%% +-------------------------------- - - - - - - - - - - - - - - - +
%% :                     Payload Data continued ...                :
%% + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%% |                     Payload Data continued ...                |
%% +---------------------------------------------------------------+


parse_received_data(<<1:1, 0:3, ?OP_TEXT:4, 0:1, Len:7, BData/binary>>) when Len < 126 ->
   {?OP_TEXT,  binary_to_list(BData)};
parse_received_data(Bin) ->
    {error, "not implemented yet"}.















%% -record(http_message,{
%%     type :: response | request,
%%     start_line :: list({atom(), string()}),
%%     headers :: list({atom(), string()})
%%   }).

%% -record(handshake, {
%%     version      :: integer(),
%%     message :: #http_message{}
%%   }).

%% -type bit() :: 0..1.

%% -record(frame, {
%%     fin = 0:: bit(),
%%     rsv1 = 0 :: bit(),
%%     rsv2 = 0 :: bit(),
%%     rsv3 = 0 :: bit(),
%%     opcode :: byte(),
%%     mask :: bit(),
%%     payload_len :: byte(),
%%     extended_payload_len :: byte(),
%%     extended_payload_len_cont :: integer(),
%%     masking_key :: binary(),
%%     payload :: binary()}).

%% -record(message, {
%%     frames = [] :: list(#frame{}),
%%     payload :: string() | binary(), % FALSE!!! what about control message with code + message
%%     type :: {text, binary, control, fragmented}
%%   }).









%% -spec encode(Data::string() | binary(), Type::atom()) -> binary().
%% encode(Data, Type) when is_list(Data)->
%%   encode(list_to_binary(Data), Type);

%% encode(Data, Type)->
%%   lists:reverse(encode(Data, Type, [])).

%% -spec decode(Data::binary()) -> list(#message{}).
%% decode(Data) ->
%%   decode(Data, begin_message, #message{}).

%% -spec decode(Data::binary(), Message::#message{}) -> list(#message{}).
%% decode(Data, Message) ->
%%   decode(Data, continue_message, Message).


%% %
%% % Internal
%% %
%% -spec encode(Data::binary(), Type :: atom(), Acc ::list()) -> list().
%% encode(Data, Type, _Acc) when Type =:= ping ; Type =:= pong ; Type =:= close->
%%   [frame(Data, [fin, {opcode, Type}])];
%%   %Frame = wsecli_framing:frame(Data, [fin, {opcode, Type}]),
%%   %wsecli_framing:to_binary(Frame);

%% encode(<<Data:?FRAGMENT_SIZE/binary>>, Type, Acc) ->
%%   [frame(Data, [fin, {opcode, Type}]) | Acc];

%% encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, []) ->
%%   encode(Rest, continuation, [frame(Data, [{opcode, Type}]) | []]);

%% encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, Acc) ->
%%   encode(Rest, Type, [frame(Data, [{opcode, Type}]) | Acc]);

%% encode(<<>>, _Type, Acc) ->
%%   Acc;

%% encode(<<Data/binary>>, Type, Acc) ->
%%   [frame(Data, [fin, {opcode, Type}]) | Acc].




%% -spec decode(Data::binary(), Type :: message_type(), Message::#message{}) -> list(#message{}).
%% decode(Data, begin_message, _Message) ->
%%   Frames = wsecli_framing:from_binary(Data),
%%   lists:reverse(process_frames(begin_message, Frames, []));

%% decode(Data, continue_message, Message) ->
%%   Frames = wsecli_framing:from_binary(Data),
%%   lists:reverse(process_frames(continue_message, Frames, [Message | []])).


%% -spec frame(Data::binary(), Options::list()) -> binary().
%% frame(Data, Options) ->
%%   Frame = wsecli_framing:frame(Data, Options),
%%   wsecli_framing:to_binary(Frame).
