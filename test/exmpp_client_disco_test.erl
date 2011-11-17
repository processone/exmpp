-module(exmpp_client_disco_test).

-include_lib("eunit/include/eunit.hrl").

info_test() ->
	R = exmpp_client_disco:info(<<"domain.com">>),
	?assertEqual(<<"get">>, exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(<<"domain.com">>, exml:get_attribute(R, <<"to">>, undefined)),
	?assertEqual(<<"http://jabber.org/protocol/disco#info">>, 
		exml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),

	R2 = exmpp_client_disco:info(<<"domain.com">>, <<"node">>),
	?assertEqual(<<"get">>, exml:get_attribute(R2, <<"type">>, undefined)),
	?assertEqual(<<"domain.com">>, exml:get_attribute(R2, <<"to">>, undefined)),
	?assertEqual(<<"http://jabber.org/protocol/disco#info">>, 
		exml:get_path(R2, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"node">>, 
		exml:get_path(R2, [{element, <<"query">>}, {attribute, <<"node">>}])),

	ok.

items_test() ->
	R = exmpp_client_disco:items(<<"domain.com">>),
	?assertEqual(<<"get">>, exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(<<"domain.com">>, exml:get_attribute(R, <<"to">>, undefined)),
	?assertEqual(<<"http://jabber.org/protocol/disco#items">>, 
		exml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),

	R2 = exmpp_client_disco:items(<<"domain.com">>, <<"node">>),
	?assertEqual(<<"get">>, exml:get_attribute(R2, <<"type">>, undefined)),
	?assertEqual(<<"domain.com">>, exml:get_attribute(R2, <<"to">>, undefined)),
	?assertEqual(<<"http://jabber.org/protocol/disco#items">>, 
		exml:get_path(R2, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"node">>, 
		exml:get_path(R2, [{element, <<"query">>}, {attribute, <<"node">>}])),

	ok.
