%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <mail@pablovieytes.com>
%%% @copyright (C) 2012, Pablo Vieytes
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%%-------------------------------------------------------------------

-module(ewsclient_ws13).
-include("include/ewsclient.hrl").
-include_lib("wsock/include/wsock.hrl").


%% API
-export([create_handshake_req/3,
	 check_handshake_server_response/1,
	 parse_received_data/1
	]).




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create handshake (vs. 13) request
%%
%% @spec create_handshake_req(Host::sting(), Port::integer(), Path::string()) ->
%%                  ok | error
%%                                   
%% @end
%%--------------------------------------------------------------------
create_handshake_req(Host, Port, Path)->
    PortStr = io_lib:format("~p", [Port]),
    Key = binary_to_list(base64:encode(crypto:rand_bytes(16))),
    lists:flatten("GET " ++ Path ++ " HTTP/1.1\r\n" ++ 
    		      "Upgrade: WebSocket\r\n" ++
    		      "Connection: Upgrade\r\n" ++
    		      "Host: " ++ Host ++ ":" ++ PortStr ++ "\r\n" ++
    		      "Origin: " ++ Host ++ ":" ++ PortStr ++ "\r\n" ++
    		      "Sec-WebSocket-Key: " ++ Key ++ "\r\n" ++
    		      "Sec-WebSocket-Version: 13\r\n" ++
    		      "\r\n").




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the handshake response of the server
%%
%% @spec check_handshake_server_response([{HeaderKey::atom(), Value::string()]) ->
%%                  ok | error
%%                                   
%% @end
%%--------------------------------------------------------------------
check_handshake_server_response(Headers)->
    Connection = proplists:get_value('Connection', Headers),
    ConnectionLower = string:to_lower(Connection),
    if
	ConnectionLower == "upgrade" ->
	    Upgrade = proplists:get_value('Upgrade', Headers),
	    UpgradeLower = string:to_lower(Upgrade),
	    if 
		UpgradeLower == "websocket" ->
		    ok;
		true ->
		    error
	    end;
	true ->
	    error
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parse data received
%%
%% @spec parse_received_data(Data::binary()) ->  {?OP_TEXT,  string()} | {error, Error}
%%                                   
%% @end
%%--------------------------------------------------------------------

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
parse_received_data(_Bin) ->
    {error, "not implemented yet"}.






