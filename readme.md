
## ewsclient 
ewsclient is a websocket client written in erlang.

**License:** 

**Comments to:** mail@pablovieytes.com

**Current Version:** 0.1


### Rebar
This project use rebar to compile it. Please check how to use rebar.  https://github.com/basho/rebar


### test 
run the test.
```shell
make test
```


## Usage Examples

Start erlang and ewsclient.
```shell
$ cd /path/to/project/
$ rebar get-deps
$ rebar compile
$ erl  -pa ./ebin -s ewsclient
```


```erlang
1> {ok, Ws} = ewsclient:start_client().
2> Ws:connect("ws://echo.websocket.org").
3> Ws:send("data string").
```
