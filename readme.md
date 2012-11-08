## igel 
**igel** is a multi websocket client written in erlang.

**License:** 

**Comments to:** mail@pablovieytes.com

**Current Version:** 0.1

### Compile

```shell
make
```
This project uses rebar to compile it. If you want more info about it, please, check https://github.com/basho/rebar


### Test 
run the tests.
```shell
make test
```

## Usage Examples

Start igel and erlang console.
```shell
$ cd /path/to/project/
$ make
$ erl -pa ebin -pa deps/wsock/ebin -pa deps/wsock -pa deps/wsock/include -s igel
```
erlang shell:

```erlang

1> {ok, Ws} = igel:start_client().
{ok,{igel,<0.39.0>}}
2> Ws:connect("ws://echo.websocket.org").
default on_open.
ok
3> Ws:send("data string").
ok
default on_msg :: receive: "data string"
4> FOnMsg = fun(Msg) -> io:format("new on msg: received: ~p~n", [Msg]) end.
#Fun<erl_eval.6.80247286>
5> Ws:override_callback({on_msg, FOnMsg}).          
ok
6> Ws:send("string").
ok
new on msg: received: "string"
7> Host = "ws://echo.websocket.org".
"ws://echo.websocket.org"
8> FOnOpen = fun() -> io:format("new on open fun~n") end.
#Fun<erl_eval.20.21881191>
9> Parmas = [{connect, Host},{callbacks,[{on_open, FOnOpen},{on_msg, FOnMsg}]}].
10> {ok, Ws2} = igel:start_client(Parmas).
new on open fun
{ok,{igel,<0.52.0>}}
11> Ws2:send("string").
ok
new on msg: received: "string"
```


###Start local server for testing
How to run a mirror websocket server on ws://localhost:8080/websocket

```shell
$ cd /path/to/project/
$ cd test
$ ../rebar get deps
$ ../rebar compile
$ erl -pa ebin -pa deps/cowboy/ebin -s wstestserver
```

You can use a html websocket client on http://localhost:8080/websocket to test the websocket test server.



