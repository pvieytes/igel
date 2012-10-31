all:
	rebar get-deps
	rebar compile

clean:
	rebar clean

test: all
	rebar compile
	(cd test; erlc ewsclient_tests.erl)
	erl -noshell -pa ebin -pa test -pa deps/wsock/ebin -s ewsclient_tests -s erlang halt

