-module(exmpp_client_legacy_auth_test).


-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

request_test() ->
	R = exmpp_client_legacy_auth:request(<<"domain.com">>),
	?assertEqual(?NS_LEGACY_AUTH, 
		exxml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	ok.

request_with_user_test() ->
	R = exmpp_client_legacy_auth:request_with_user(<<"domain.com">>, <<"user">>),
	?assertEqual(?NS_LEGACY_AUTH, 
		exxml:get_path(R, [{element, <<"query">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"user">>,
		exxml:get_path(R, [{element, <<"query">>}, {element, <<"username">>},cdata])),
	ok.

password_test() ->
	Fields = {xmlel, <<"iq">>, [{<<"type">>, <<"result">>}], [{xmlel, <<"query">>, [], 
		[
			{xmlel, <<"username">>, [], []},
			{xmlel, <<"password">>, [], []},
			{xmlel, <<"resource">>, [], []}
		]}]},
	R = exmpp_client_legacy_auth:password(Fields, <<"user">>, <<"password">>, <<"resource">>),
	?assertEqual(<<"user">>, exxml:get_path(R, [{element, <<"query">>}, {element, <<"username">>}, cdata])),
	?assertEqual(<<"resource">>, exxml:get_path(R, [{element, <<"query">>}, {element, <<"resource">>}, cdata])),
	ok.

password_digest_test() ->
	Fields = {xmlel, <<"iq">>, [{<<"type">>, <<"result">>}], [{xmlel, <<"query">>, [], 
		[
			{xmlel, <<"username">>, [], []},
			{xmlel, <<"password">>, [], []},
			{xmlel, <<"resource">>, [], []},
			{xmlel, <<"digest">>, [], []}
		]}]},
	%% example ID, password, and digest taken from XEP-0078
	R = exmpp_client_legacy_auth:password(Fields, <<"user">>, <<"Calli0pe">>, <<"resource">>, <<"3EE948B0">>),
	?assertEqual(<<"user">>, exxml:get_path(R, [{element, <<"query">>}, {element, <<"username">>}, cdata])),
	?assertEqual(<<"resource">>, exxml:get_path(R, [{element, <<"query">>}, {element, <<"resource">>}, cdata])),
	?assertEqual(<<"48fc78be9ec8f86d8ce1c39c320c97c21d62334d">>, 
		exxml:get_path(R, [{element, <<"query">>}, {element, <<"digest">>}, cdata])),
	ok.


