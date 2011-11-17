-module(exmpp_client_muc_test).

-include_lib("eunit/include/eunit.hrl").
-include("exmpp.hrl").

kick_test() ->
	R = exmpp_client_muc:kick(<<"room@conf.domain.com">>, <<"nick">>, <<"out">>),
	?assertEqual(<<"set">>, exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(?NS_MUC_ADMIN, exml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"nick">>, exml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"nick">>}])),
	?assertEqual(<<"none">>, exml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"role">>}])),
	?assertEqual(<<"out">>, exml:get_path(R, [{element, <<"query">>},
				{element, <<"item">>}, {element, <<"reason">>}, cdata])),
	ok.

ban_test() ->
	R = exmpp_client_muc:ban(<<"room@conf.domain.com">>, <<"nick">>, <<"out">>),
	?assertEqual(<<"set">>, exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(?NS_MUC_ADMIN, exml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"outcast">>, exml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"affiliation">>}])),
	?assertEqual(<<"out">>, exml:get_path(R, [{element, <<"query">>},
				{element, <<"item">>}, {element, <<"reason">>}, cdata])),
	ok.

get_banlist_test() ->
	R = exmpp_client_muc:get_banlist(<<"room@conf.domain.com">>),
	?assertEqual(?NS_MUC_ADMIN, exml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"get">>, exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(<<"outcast">>, exml:get_path(R, [{element, <<"query">>}, {element, <<"item">>}, {attribute, <<"affiliation">>}])),
	ok.

