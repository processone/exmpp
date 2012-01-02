-module(exmpp_stream_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

opening_test() ->
	O = exmpp_stream:opening(<<"domain.com">>, <<"jabber:client">>, {1,0}),
	?assertEqual(<<"domain.com">>, exmpp_stream:get_receiving_entity(O)),
	?assertEqual(<<"jabber:client">>, exmpp_stream:get_default_ns(O)),
	?assertEqual({1,0}, exmpp_stream:get_version(O)),
	?assertEqual(<<"http://etherx.jabber.org/streams">>, exxml:get_attribute(O,<<"xmlns:stream">>)),
	?assertMatch({xmlel, <<"stream:stream">>, _,undefined}, O),
	S = exxml:document_to_iolist(O),
	{ok, P} = exxml:start_parser([{root_depth, 1}]),
	{ok, [O2]} = exxml:parse(P,iolist_to_binary(S)),
	?assertEqual(<<"domain.com">>, exmpp_stream:get_receiving_entity(O2)),
	?assertEqual(<<"jabber:client">>, exmpp_stream:get_default_ns(O2)),
	?assertEqual({1,0}, exmpp_stream:get_version(O2)),
	?assertMatch({xmlel, <<"stream:stream">>, _,undefined}, O2),

	?assertEqual(<<"EN">>, 
		exmpp_stream:get_lang(exmpp_stream:opening(<<"domain.com">>, <<"jabber:client">>, undefined, <<"EN">>))),
	?assertEqual({0,0}, 
		exmpp_stream:get_version(exmpp_stream:opening(<<"domain.com">>, <<"jabber:client">>, undefined))),

	ok.

opening_reply_test() ->
	O = exmpp_stream:opening_reply(<<"domain.com">>, <<"jabber:client">>, {1,0}, <<"id">>, <<"EN">>),
	?assertEqual(<<"domain.com">>, exmpp_stream:get_initiating_entity(O)),
	?assertEqual(<<"jabber:client">>, exmpp_stream:get_default_ns(O)),
	?assertEqual({1,0}, exmpp_stream:get_version(O)),
	?assertEqual(<<"id">>, exmpp_stream:get_id(O)),
	?assertMatch({xmlel, <<"stream:stream">>, _,undefined}, O),
	?assertEqual(<<"EN">>, exmpp_stream:get_lang(O)),
	?assertEqual(<<"http://etherx.jabber.org/streams">>, exxml:get_attribute(O,<<"xmlns:stream">>)),
	ok.

opening_reply_from_opening_test() ->
	O = exmpp_stream:opening_reply(
		exmpp_stream:opening(<<"domain.com">>, <<"jabber:client">>, {1,0}, <<"id">>),<<"aa">>),
	?assertEqual(<<"domain.com">>, exmpp_stream:get_initiating_entity(O)),
	?assertEqual(<<"jabber:client">>, exmpp_stream:get_default_ns(O)),
	?assertEqual({1,0}, exmpp_stream:get_version(O)),
	?assertEqual(<<"aa">>, exmpp_stream:get_id(O)),
	?assertMatch({xmlel, <<"stream:stream">>, _,undefined}, O),
	ok.

closing_test() ->
	?assertMatch({xmlelend, <<"stream:stream">>}, exmpp_stream:closing()),
	?assertMatch({xmlelend, <<"stream:stream">>}, 
		exmpp_stream:closing(exmpp_stream:opening(<<"domain.com">>, <<"jabber:client">>, {1,0}, <<"id">>))),
	ok.

features_test() ->
	F = exmpp_stream:features([{xmlel, <<"a">>, [], []}, {xmlel, <<"b">>, [], []}]),
	?assertMatch({xmlel, <<"stream:features">>, _, _} , F),
	?assertMatch(?NS_XMPP, exxml:get_attribute(F, <<"xmlns:stream">>)),
	?assertEqual(2, length(exxml:get_elements(F))),
	ok.

error_test() ->
	E = exmpp_stream:error(<<"xml-not-well-formed">>),
	?assertMatch({xmlel, <<"stream:error">>, _, _}, E),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-streams">>, 
		exxml:get_path(E,[{element, <<"xml-not-well-formed">>}, {attribute, <<"xmlns">>}])),

	E2 = exmpp_stream:error(<<"xml-not-well-formed">>, {<<"EN">>, <<"bad xml">>}),
	?assertMatch({xmlel, <<"stream:error">>, _, _}, E2),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-streams">>, 
		exxml:get_path(E2,[{element, <<"xml-not-well-formed">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-streams">>, 
		exxml:get_path(E2,[{element, <<"text">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"bad xml">>, 
		exxml:get_path(E2,[{element, <<"text">>}, cdata])),
	?assertEqual(<<"bad xml">>, exmpp_stream:get_text(E2)),
	?assertEqual(<<"xml-not-well-formed">>, exmpp_stream:get_condition(E2)),
	ok.

dialback_support_test() ->
	E = exmpp_stream:set_dialback_support(
		exmpp_stream:opening(<<"domain.com">>, <<"jabber:server">>, undefined)),
	?assertEqual(<<"jabber:server:dialback">>, exxml:get_attribute(E, <<"xmlns:db">>)),
	ok.
