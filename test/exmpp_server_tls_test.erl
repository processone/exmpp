-module(exmpp_server_tls_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


feature_test() ->
	B = exmpp_server_tls:feature(),
	?assertMatch({xmlel, <<"starttls">>, _, []}, B),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-tls">>, exml:get_attribute(B, <<"xmlns">>)),
	?assertMatch([{xmlel, <<"required">>, _, _}], exml:get_elements(exmpp_server_tls:feature(true))),
	ok.

procees_test() ->
	B = exmpp_server_tls:proceed(),
	?assertMatch({xmlel, <<"proceed">>, _, []}, B),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-tls">>, exml:get_attribute(B, <<"xmlns">>)),
	ok.
failure_test() ->
	B = exmpp_server_tls:failure(),
	?assertMatch({xmlel, <<"failure">>, _, []}, B),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-tls">>, exml:get_attribute(B, <<"xmlns">>)),
	ok.
