%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).


-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(Req, State) ->

    Html = << "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
    <title>Websocket client</title>
	      <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.8.0/jquery.min.js\"></script>
    <script type=\"text/javascript\">
      var websocket;

	      $(document).ready(init);

      function init() {
          if(!(\"WebSocket\" in window)){  
              $('#status').append('<p><span style=\"color: red;\">web sockets are not supported </span></p>');
              $(\"#navigation\").hide();  
          }
          else
          {
              $('#status').append('<p><span style=\"color: green;\">web sockets are supported </span></p>');
          }
          $(\"#connected\").hide(); 	
          $(\"#content\").hide(); 	
      };

      function connect()
      {

          wsHost = $(\"#server\").val()
          websocket = new WebSocket(wsHost);
          showScreen('<b>Connecting to: ' +  wsHost + '</b>'); 

          websocket.onopen = function(evt) { onOpen(evt) }; 
          websocket.onclose = function(evt) { onClose(evt) }; 
          websocket.onmessage = function(evt) { onMessage(evt) }; 
          websocket.onerror = function(evt) { onError(evt) }; 
      };  

      
      function disconnect()
      {
          websocket.close();
      }; 


      function sendTxt()
      {
         txt = $(\"#send_txt\").val();
         websocket.send(txt);
         showScreen('sending: ' + txt); 
      };

      function onOpen(evt) 
      { 
            showScreen('<span style=\"color: green;\">CONNECTED </span>'); 
            $(\"#connected\").fadeIn('slow');
            $(\"#content\").fadeIn('slow');
      };  


      function onClose(evt) 
      { 
          showScreen('<span style=\"color: red;\">DISCONNECTED </span>');
      };  


      function onMessage(evt) 
      { 
          showScreen('<span style=\"color: blue;\">RESPONSE: ' + evt.data+ '</span>'); 
      };  


      function showScreen(txt) 
      { 
           $('#output').prepend('<p>' + txt + '</p>');
      };

    </script>
  </head>

  <body>
    <div id=\"header\">
      <h1>Websocket client</h1>
      <div id=\"status\"></div>
    </div>


    <div id=\"navigation\">

      <p id=\"connecting\">
	<input type='text' id=\"server\" value=\"ws://localhost:8080/websocket\"></input>
	<button type=\"button\" onclick=\"connect()\">connect</button>
	<button type=\"button\" onclick=\"disconnect()\">disconnect</button>
      </p>
      <div id=\"connected\">				
	<p>
	  <input type='text' id=\"send_txt\" value=""></input>
	  <button type=\"button\" onclick=\"sendTxt();\">send</button>
	</p>
      </div>

      <div id=\"content\">						
	<button id=\"clear\" onclick=\"clearScreen()\" >Clear text</button>
	<div id=\"output\"></div>
      </div>

    </div>
  </body>
</html> ">>,

    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}], Html, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

websocket_init(_Any, Req, []) ->
	Req2 = cowboy_http_req:compact(Req),
	{ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << Msg/binary >>}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
