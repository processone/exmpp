-module(exmpp_stanza_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


error_test() ->
   S = <<"<iq type='error' id='some-id'> <error type='modify'> ",
         "<bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>",
   	" <too-many-parameters xmlns='application-ns'/> </error> </iq>">>,
   {ok, [E]} = exml:parse_document(S),
   ?assertEqual(<<"modify">>, exmpp_stanza:get_error_type(E)),
   ?assertEqual(exmpp_stanza:get_error(E), exmpp_stanza:get_error(exmpp_iq:xmlel_to_iq(E))),
   ?assertEqual(<<"bad-request">>, exmpp_stanza:get_condition(E)),
   ok.

remove_sender_test() ->
	M = {xmlel, <<"message">>, [{<<"from">>, <<"domain.com">>}], []},
	?assertEqual(<<"domain.com">>, exmpp_stanza:get_sender(M)),
	?assertEqual(undefined, exmpp_stanza:get_sender(exmpp_stanza:remove_sender(M))),
	ok.

remove_recipient_test() ->
	M = {xmlel, <<"message">>, [{<<"to">>, <<"domain.com">>}], []},
	?assertEqual(<<"domain.com">>, exmpp_stanza:get_recipient(M)),
	?assertEqual(undefined, exmpp_stanza:get_recipient(exmpp_stanza:remove_recipient(M))),
	ok.

set_jids_test() ->
	M = exmpp_stanza:set_jids({xmlel, <<"message">>, [{<<"to">>, <<"domain.com">>}], []},<<"from.com">>, <<"to.com">>),
	?assertEqual(<<"from.com">>, exmpp_stanza:get_sender(M)),
	?assertEqual(<<"to.com">>, exmpp_stanza:get_recipient(M)),
	ok.

id_test() ->
	M = {xmlel, <<"iq">>, [{<<"id">>, <<"some">>}, {<<"type">>, <<"get">>}], 
		[{xmlel, <<"query">>, [{<<"xmlns">>, <<"http://jabber.org/protocol/disco#info">>}], []}]},
	?assertEqual(<<"some">>, exmpp_stanza:get_id(M)),
	?assertEqual(<<"other">>, exmpp_stanza:get_id(exmpp_stanza:set_id(M,<<"other">>))),
	?assertEqual(<<"some">>, exmpp_stanza:get_id(exmpp_iq:xmlel_to_iq(M))),
	ok.

type_test() ->
	M = {xmlel, <<"iq">>, [{<<"id">>, <<"some">>}, {<<"type">>, <<"get">>}], 
		[{xmlel, <<"query">>, [{<<"xmlns">>, <<"http://jabber.org/protocol/disco#info">>}], []}]},
	?assertEqual(<<"get">>, exmpp_stanza:get_type(M)),
	?assertEqual(<<"set">>, exmpp_stanza:get_type(exmpp_stanza:set_type(M,<<"set">>))),
	?assertEqual(<<"get">>, exmpp_stanza:get_type(exmpp_iq:xmlel_to_iq(M))),
	ok.
