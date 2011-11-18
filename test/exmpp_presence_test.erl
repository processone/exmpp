-module(exmpp_presence_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


presence_test() ->
	P = exmpp_presence:presence(<<"available">>, <<"casa">>),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"available">>, exmpp_presence:get_type(P)),
	?assertEqual(<<"casa">>, exmpp_presence:get_status(P)),
	ok.

available_test() ->
	P = exmpp_presence:available(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"available">>, exmpp_presence:get_type(P)),
	ok.

unavailable_test() ->
	P = exmpp_presence:unavailable(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"unavailable">>, exmpp_presence:get_type(P)),
	ok.

subscribe_test() ->
	P = exmpp_presence:subscribe(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"subscribe">>, exmpp_presence:get_type(P)),
	ok.
subscribed_test() ->
	P = exmpp_presence:subscribed(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"subscribed">>, exmpp_presence:get_type(P)),
	ok.
unsubscribe_test() ->
	P = exmpp_presence:unsubscribe(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"unsubscribe">>, exmpp_presence:get_type(P)),
	ok.
unsubscribed_test() ->
	P = exmpp_presence:unsubscribed(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"unsubscribed">>, exmpp_presence:get_type(P)),
	ok.
probe_test() ->
	P = exmpp_presence:probe(),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"probe">>, exmpp_presence:get_type(P)),
	ok.

error_test() ->
	P = exmpp_presence:error(exmpp_presence:probe(), <<"forbidden">>),
	?assertMatch({xmlel, <<"presence">>, _,_} , P),
	?assertEqual(<<"error">>, exmpp_presence:get_type(P)),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>, 
		exml:get_path(P, [{element, <<"error">>}, {element, <<"forbidden">>},{attribute, <<"xmlns">>}])),
	ok.

is_presence_test() ->
	?assert(exmpp_presence:is_presence(exmpp_presence:available())),
	ok.

set_type_test() ->
	P = exmpp_presence:set_type(exmpp_presence:available(), <<"unavailable">>),
	?assertEqual(<<"unavailable">>, exmpp_presence:get_type(P)),
	?assertEqual(<<"available">>, exmpp_presence:get_type(exmpp_presence:set_type(P, <<"available">>))),
	ok.

show_test() ->
	P = exmpp_presence:set_show(exmpp_presence:available(), <<"xa">>),
	?assertEqual(<<"xa">>, exmpp_presence:get_show(P)),
	?assertEqual(<<"away">>, exmpp_presence:get_show(exmpp_presence:set_show(P, <<"away">>))),
	ok.

status_test() ->
	P = exmpp_presence:set_status(exmpp_presence:available(), <<"xa">>),
	?assertEqual(<<"xa">>, exmpp_presence:get_status(P)),
	?assertEqual(<<"away">>, exmpp_presence:get_status(exmpp_presence:set_status(P, <<"away">>))),
	ok.

priority_test() ->
	P = exmpp_presence:available(),
	?assertEqual(0, exmpp_presence:get_priority(P)),
	?assertEqual(10, exmpp_presence:get_priority(exmpp_presence:set_priority(P, 10))),
	ok.

