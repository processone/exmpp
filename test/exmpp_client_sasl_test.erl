-module(exmpp_client_sasl_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

announced_mechanism_test() ->
     S = <<"<stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>",
       "<mechanism>EXTERNAL</mechanism> <mechanism>SCRAM-SHA-1-PLUS</mechanism>",
       "<mechanism>SCRAM-SHA-1</mechanism> <mechanism>PLAIN</mechanism>",
     "</mechanisms></stream:features>">>,
     {ok, [M]} = exxml:parse_document(S),
     R = exmpp_client_sasl:announced_mechanisms(M),
     ?assertEqual(4, length(R)),
     ?assert(lists:member(<<"EXTERNAL">>, R)),
     ?assert(lists:member(<<"SCRAM-SHA-1-PLUS">>, R)),
     ?assert(lists:member(<<"SCRAM-SHA-1">>, R)),
     ?assert(lists:member(<<"PLAIN">>, R)),
     ok.

selected_mechanism1_test() ->
	R = exmpp_client_sasl:selected_mechanism(<<"PLAIN">>),
	?assertEqual(?NS_SASL, exxml:get_attr(R, <<"xmlns">>)),
	?assertEqual(<<"PLAIN">>, exxml:get_attr(R, <<"mechanism">>)),
	ok.

selected_mechanism2_test() ->
	R = exmpp_client_sasl:selected_mechanism(<<"PLAIN">>, <<>>),
	?assertEqual(?NS_SASL, exxml:get_attr(R, <<"xmlns">>)),
	?assertEqual(<<"PLAIN">>, exxml:get_attr(R, <<"mechanism">>)),
	?assertEqual(<<"=">>, exxml:get_cdata(R)),
	?assertEqual(?NS_SASL, exxml:get_attr(R, <<"xmlns">>)),
	?assertEqual(<<"PLAIN">>, exxml:get_attr(R, <<"mechanism">>)),
	?assertEqual(base64:encode(<<"test">>), 
		exxml:get_cdata(exmpp_client_sasl:selected_mechanism(<<"PLAIN">>, <<"test">>))),
	ok.


response_test() ->
	R = exmpp_client_sasl:response(<<"aa">>),
	?assertEqual(?NS_SASL, exxml:get_attr(R, <<"xmlns">>)),
	?assertEqual(<<"aa">>, base64:decode(exxml:get_cdata(R))),
	ok.

abort_test() ->
	?assertMatch({xmlel, <<"abort">>, [{<<"xmlns">>, ?NS_SASL}], []}, exmpp_client_sasl:abort()),
	ok.

next_step_failure_test() ->
	{ok, [F]} = exxml:parse_document(
		<<"<failure xmlns='urn:ietf:params:xml:ns:xmpp-sasl'> <not-authorized/> </failure>">>),
	?assertMatch({failure, <<"not-authorized">>}, exmpp_client_sasl:next_step(F)),
	ok.

next_step_success_test() ->
	{ok, [F]} = exxml:parse_document(
		<<"<success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>">>),
	?assertMatch({success, <<>>}, exmpp_client_sasl:next_step(F)),
	ok.

next_step_challenge_test() ->
	%% Values taken from RFC6120
	{ok, [F]} = exxml:parse_document(
		<<"<challenge xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>",
     "cj1vTXNUQUF3QUFBQU1BQUFBTlAwVEFBQUFBQUJQVTBBQWUxMjQ2OTViLTY5Y",
     "TktNGRlNi05YzMwLWI1MWIzODA4YzU5ZSxzPU5qaGtZVE0wTURndE5HWTBaaT",
     "AwTmpkbUxUa3hNbVV0TkRsbU5UTm1ORE5rTURNeixpPTQwOTY=",
     "</challenge>">>),
	Expected = <<"r=oMsTAAwAAAAMAAAANP0TAAAAAABPU0AAe124695b-69a9-4de6-9c30-b51b3808c59e,s=NjhkYTM0MDgtNGY0Zi00NjdmLTkxMmUtNDlmNTNmNDNkMDMz,i=4096">>,
	?assertMatch({challenge, Expected}, exmpp_client_sasl:next_step(F)),
	ok.

