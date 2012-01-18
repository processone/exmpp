all: compile eunit

compile:
	./rebar compile

eunit:
	./rebar eunit

clean:
	./rebar clean
	rm -rf priv ebin .eunit
