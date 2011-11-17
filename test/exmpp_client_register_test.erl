-module(exmpp_client_register_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

get_registration_fields_test() ->
	R = exmpp_client_register:get_registration_fields(),
	?assertEqual(<<"get">>,exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(?NS_INBAND_REGISTER, exml:get_path(R,[{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	ok.



register_account_test() ->
	Fields = [{<<"password">>, <<"pass">>}, {<<"username">>, <<"user">>}],
	R = exmpp_client_register:register_account(Fields),
	?assertEqual(<<"set">>,exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(?NS_INBAND_REGISTER, exml:get_path(R,[{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"pass">>, exml:get_path(R,[{element, <<"query">>}, {element, <<"password">>}, cdata])),
	?assertEqual(<<"user">>, exml:get_path(R,[{element, <<"query">>}, {element, <<"username">>}, cdata])),

	ok.

remove_account_test() ->
	R = exmpp_client_register:remove_account(),
	?assertEqual(<<"set">>,exml:get_attribute(R, <<"type">>, undefined)),
	?assertEqual(?NS_INBAND_REGISTER, exml:get_path(R,[{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertMatch({xmlel, <<"remove">>, [], []}, exml:get_path(R,[{element, <<"query">>}, {element, <<"remove">>}])),
	ok.
