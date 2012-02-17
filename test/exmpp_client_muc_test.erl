-module(exmpp_client_muc_test).

-include_lib("eunit/include/eunit.hrl").
-include("exmpp.hrl").

kick_test() ->
	R = exmpp_client_muc:kick(<<"room@conf.domain.com">>, <<"nick">>, <<"out">>),
	?assertEqual(<<"set">>, exxml:get_attr(R, <<"type">>, undefined)),
	?assertEqual(?NS_MUC_ADMIN, exxml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"nick">>, exxml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"nick">>}])),
	?assertEqual(<<"none">>, exxml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"role">>}])),
	?assertEqual(<<"out">>, exxml:get_path(R, [{element, <<"query">>},
				{element, <<"item">>}, {element, <<"reason">>}, cdata])),
	ok.

ban_test() ->
	R = exmpp_client_muc:ban(<<"room@conf.domain.com">>, <<"nick">>, <<"out">>),
	?assertEqual(<<"set">>, exxml:get_attr(R, <<"type">>, undefined)),
	?assertEqual(?NS_MUC_ADMIN, exxml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"outcast">>, exxml:get_path(R, [{element, <<"query">>},{element, <<"item">>}, {attribute, <<"affiliation">>}])),
	?assertEqual(<<"out">>, exxml:get_path(R, [{element, <<"query">>},
				{element, <<"item">>}, {element, <<"reason">>}, cdata])),
	ok.

get_banlist_test() ->
	R = exmpp_client_muc:get_banlist(<<"room@conf.domain.com">>),
	?assertEqual(?NS_MUC_ADMIN, exxml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"get">>, exxml:get_attr(R, <<"type">>, undefined)),
	?assertEqual(<<"outcast">>, exxml:get_path(R, [{element, <<"query">>}, {element, <<"item">>}, {attribute, <<"affiliation">>}])),
	ok.

