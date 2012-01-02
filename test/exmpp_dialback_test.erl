-module(exmpp_dialback_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


key_test() ->
	F = <<"from.com">>,
	T = <<"to.com">>,
	Key = <<"key">>,
	R = exmpp_dialback:key(F, T, Key),
	?assertEqual(F, exxml:get_attribute(R, <<"from">>)),
	?assertEqual(T, exxml:get_attribute(R, <<"to">>)),
	?assertEqual(Key, exxml:get_cdata(R)),
	?assertMatch({xmlel, <<"db:result">>, _, _}, R),
	ok.

verify_request_test() ->
	F = <<"from.com">>,
	T = <<"to.com">>,
	Key = <<"key">>,
	ID = <<"ID">>,
	R = exmpp_dialback:verify_request(F, T, ID, Key),
	?assertEqual(F, exxml:get_attribute(R, <<"from">>)),
	?assertEqual(T, exxml:get_attribute(R, <<"to">>)),
	?assertEqual(ID, exxml:get_attribute(R, <<"id">>)),
	?assertEqual(Key, exxml:get_cdata(R)),
	?assertMatch({xmlel, <<"db:verify">>, _, _}, R),
	ok.

verify_response_test() ->
	F = <<"from.com">>,
	T = <<"to.com">>,
	Key = <<"key">>,
	ID = <<"ID">>,
	Rq = exmpp_dialback:verify_request(F, T, ID, Key),
	Resp = exmpp_dialback:verify_response(Rq, true),
	?assertEqual(T, exxml:get_attribute(Resp, <<"from">>)),
	?assertEqual(F, exxml:get_attribute(Resp, <<"to">>)),
	?assertEqual(ID, exxml:get_attribute(Resp, <<"id">>)),
	?assertEqual(<<"valid">>, exxml:get_attribute(Resp, <<"type">>)),
	?assertEqual(<<"invalid">>, 
		exxml:get_attribute(exmpp_dialback:verify_response(Rq, false), <<"type">>)),
	ok.

validate_test() ->
	F = <<"from.com">>,
	T = <<"to.com">>,
	Key = <<"key">>,
	R = exmpp_dialback:validate(exmpp_dialback:key(F, T, Key)),
	?assertEqual(T, exxml:get_attribute(R, <<"from">>)),
	?assertEqual(F, exxml:get_attribute(R, <<"to">>)),
	?assertMatch({xmlel, <<"db:result">>, _, _}, R),
	?assertEqual(<<"valid">>, exxml:get_attribute(R, <<"type">>)),
	ok.

validate2_test() ->
	F = <<"from.com">>,
	T = <<"to.com">>,
	R = exmpp_dialback:validate(F, T),
	?assertEqual(F, exxml:get_attribute(R, <<"from">>)),
	?assertEqual(T, exxml:get_attribute(R, <<"to">>)),
	?assertMatch({xmlel, <<"db:result">>, _, _}, R),
	?assertEqual(<<"valid">>, exxml:get_attribute(R, <<"type">>)),
	ok.

