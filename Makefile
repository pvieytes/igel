all:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean

test: all
	(cd test; ../rebar get-deps; ../rebar compile)
	erl -noshell \
		-pa ebin \
		-pa deps/wsock/ebin \
		-pa test/ebin \
		-pa test/deps/cowboy/ebin \
		-s ewsclient_tests \
		-s erlang halt

