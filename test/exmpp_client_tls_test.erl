-module(exmpp_client_tls_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

announced_support_required_test() ->
	{ok, [F]} = exxml:parse_document(
		<<"<stream:features> <starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'>",
	          "<required/> </starttls> </stream:features>">>),
	?assertEqual(required, exmpp_client_tls:announced_support(F)),
	ok.

announced_support_optional_test() ->
	{ok, [F]} = exxml:parse_document(
		<<"<stream:features> <starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>",
	          " </stream:features>">>),
	?assertEqual(optional, exmpp_client_tls:announced_support(F)),
	ok.
announced_support_none_test() ->
	{ok, [F]} = exxml:parse_document(
		<<"<stream:features> </stream:features>">>),
	?assertEqual(none, exmpp_client_tls:announced_support(F)),
	ok.

starttls_test() ->
	R = exmpp_client_tls:starttls(),
	?assertMatch({xmlel, <<"starttls">>, _ ,_}, R),
	?assertEqual(?NS_TLS, exxml:get_attribute(R, <<"xmlns">>)),
	ok.
