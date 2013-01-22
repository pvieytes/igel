#Igel


* [What is Igel?](#about)
* [Author](#author)
* [Compile](#compile)
* [Tests](#tests)
* [Usage examples](#examples)
  * [Server for testing](#test_server)
* [License](#license)


## What is Igel? <a name="about"></a>


**Igel** is a websocket multiclient written in erlang.


## Author <a name="author"></a>

This is an [openshine](http://www.openshine.com) project developed by:
  * Pablo Vieytes


## Compile <a name="compile"></a>

```shell
$ cd /path/to/project/
$ make
```
This project uses rebar to compile it. If you want more info about it, please, check https://github.com/basho/rebar


## Test <a name="tests"></a>
run the tests.

```shell
$ make test
```

## Usage Examples <a name="examples"></a>

Start igel and an erlang console.
```shell
$ cd /path/to/project/
$ make
$ erl -pa ebin -pa deps/wsock/ebin -pa deps/wsock -s igel
```
erlang shell:

```erlang

1> {ok, Ws} = igel:start_client().
2> igel:connect(Ws, "ws://echo.websocket.org").
default on_open.
3> igel:send(Ws, "data string").
default on_msg :: receive: "data string"
4> FOnMsg = fun(Msg) -> io:format("new on msg: received: ~p~n", [Msg]) end.
5> igel:override_callback(Ws, {on_msg, FOnMsg}).          
6> igel:send(Ws, "string").
new on msg: received: "string"
7> Host = "ws://echo.websocket.org".
8> FOnOpen = fun() -> io:format("new on open fun~n") end.
9> Parmas = [{connect, Host},{callbacks,[{on_open, FOnOpen},{on_msg, FOnMsg}]}].
10> {ok, Ws2} = igel:start_client(Parmas).
new on open fun
11> igel:send(Ws2, "string").
ok
new on msg: received: "string"
```




##Start local server for testing  <a name="test_server"></a>
A websocket mirror server is included for testing. This server is a [cowboyy](https://github.com/extend/cowboy) server.
It runs on ws://localhost:8080/websocket

```shell
$ cd /path/to/project/
$ cd test
$ ../rebar get-deps
$ ../rebar compile
$ erl -pa ebin -pa deps/cowboy/ebin -s wstestserver
```

You can use a html websocket client on [http://localhost:8080/websocket](http://localhost:8080/websocket) to check the websocket test server.

## License <a name="license"></a>

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. 
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

