## ewsclient 
**ewsclient** is a websocket client written in erlang.

**License:** 

**Comments to:** mail@pablovieytes.com

**Current Version:** 0.1

### Compile

```shell
make
```
This project uses rebar to compile it. If you want more info, please, check https://github.com/basho/rebar


### Test 
run the test.
```shell
make test
```


## Usage Examples

Start erlang and ewsclient.
```shell
$ cd /path/to/project/
$ make
$ erl -pa ebin -pa deps/wsock/ebin -s ewsclient
```
erlang shell:

```erlang
1> {ok, Ws} = ewsclient:start_client().
2> Ws:connect("ws://echo.websocket.org").
ok
3> Ws:send("data string").
ok
default on_msg :: receive: "data string"
```


###Start local server for testing
Run a mirror websocket server on ws://localhost:8080/websocket

```shell
$ cd /path/to/project/
$ cd test
$ ../rebar get deps
$ ../rebar compile
$ erl -pa ebin -pa deps/cowboy/ebin -s wstestserver
```
