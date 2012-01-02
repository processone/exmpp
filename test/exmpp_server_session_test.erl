-module(exmpp_server_session_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


feature_test() ->
	B = exmpp_server_session:feature(),
	?assertMatch({xmlel, <<"session">>, _, _}, B),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-session">>, exxml:get_attribute(B, <<"xmlns">>)),
	ok.

want_establishment_test() ->
	S = <<"<iq type='set' id='1'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>">>,
	{ok, [R]} = exxml:parse_document(S),
	?assert(exmpp_server_session:want_establishment(R)),
	?assertNot(exmpp_server_session:want_establishment({xmlel, <<"iq">>, [{<<"type">>, <<"get">>}], []})),
	?assertNot(exmpp_server_session:want_establishment({xmlel, <<"iq">>, [{<<"type">>, <<"set">>}], [{xmlel, <<"query">>, [], []}]})),
	ok.


