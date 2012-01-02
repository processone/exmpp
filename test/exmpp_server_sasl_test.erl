-module(exmpp_server_sasl_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


feature_test() ->
	F = exmpp_server_sasl:feature([<<"EXTERNAL">>, <<"PLAIN">>]),
	?assertMatch({xmlel, <<"mechanisms">>, _, _}, F),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-sasl">>, exxml:get_attribute(F, <<"xmlns">>)),
	?assertEqual([<<"EXTERNAL">>, <<"PLAIN">>],
		[ exxml:get_cdata(M) ||{xmlel, <<"mechanism">>, _, _} =M <- exxml:get_elements(F)]),
	ok.


challenge_test() ->
	?assertMatch({xmlel, <<"challenge">>, _, []} , exmpp_server_sasl:challenge(none)),
	C = exmpp_server_sasl:challenge(<<"aaa">>),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-sasl">>, exxml:get_attribute(C, <<"xmlns">>)),
	?assertEqual(base64:encode(<<"aaa">>), exxml:get_cdata(C)),
	ok.

success_test() ->
	?assertMatch({xmlel, <<"success">>, _, []} , exmpp_server_sasl:success(none)),
	C = exmpp_server_sasl:success(<<"aaa">>),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-sasl">>, exxml:get_attribute(C, <<"xmlns">>)),
	?assertEqual(base64:encode(<<"aaa">>), exxml:get_cdata(C)),
	ok.

failure_test() ->
	?assertMatch({xmlel, <<"failure">>, _, _}, exmpp_server_sasl:failure()),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-sasl">>, 
		exxml:get_attribute(exmpp_server_sasl:failure(), <<"xmlns">>)),
	?assertMatch({xmlel, <<"failure">>, _, _}, exmpp_server_sasl:failure(<<"not-authorized">>)),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-sasl">>, 
		exxml:get_attribute(exmpp_server_sasl:failure(<<"not-authorized">>), <<"xmlns">>)),
	?assertMatch([{xmlel, <<"not-authorized">>, [], []}],
		exxml:get_elements(exmpp_server_sasl:failure(<<"not-authorized">>))),

	?assertMatch(<<"hey">>,
		exxml:get_path(exmpp_server_sasl:failure(<<"not-authorized">>, <<"hey">>), [{element, <<"text">>},cdata])),
	ok.

next_step1_test() ->
	S = <<"<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl'
         mechanism='SCRAM-SHA-1'>
     biwsbj1qdWxpZXQscj1vTXNUQUF3QUFBQU1BQUFBTlAwVEFBQUFBQUJQVTBBQQ==
   </auth>">>,
	{ok, [R]} = exxml:parse_document(S),
	?assertEqual({auth, <<"SCRAM-SHA-1">>, <<"n,,n=juliet,r=oMsTAAwAAAAMAAAANP0TAAAAAABPU0AA">>}, exmpp_server_sasl:next_step(R)),
	ok.

next_step2_test() ->
	S = <<"<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
     Yz1iaXdzLHI9b01zVEFBd0FBQUFNQUFBQU5QMFRBQUFBQUFCUFUwQUFlMTI0N
     jk1Yi02OWE5LTRkZTYtOWMzMC1iNTFiMzgwOGM1OWUscD1VQTU3dE0vU3ZwQV
     RCa0gyRlhzMFdEWHZKWXc9
   </response>">>,
	{ok, [R]} = exxml:parse_document(S),
	?assertEqual({response, <<"c=biws,r=oMsTAAwAAAAMAAAANP0TAAAAAABPU0AAe124695b-69a9-4de6-9c30-b51b3808c59e,p=UA57tM/SvpATBkH2FXs0WDXvJYw=">>}, 
		exmpp_server_sasl:next_step(R)),

	ok.

next_step3_test() ->
	R = {xmlel, <<"abort">>, [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-sasl">>}], []},
	?assertEqual(abort, exmpp_server_sasl:next_step(R)),
	ok.

