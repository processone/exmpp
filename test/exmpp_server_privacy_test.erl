-module(exmpp_server_privacy_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

list_push_test() ->
	P = exmpp_server_privacy:list_push(<<"user@domain.com/resource">>, <<"public">>),
	?assertMatch({xmlel, <<"iq">>, _, _}, P),
	?assertEqual(<<"user@domain.com/resource">>, exml:get_attribute(P, <<"to">>)),
	?assertEqual(<<"set">>, exml:get_attribute(P, <<"type">>)),
	?assertEqual(<<"public">>, 
		exml:get_path(P, [{element, <<"query">>}, {element, <<"list">>}, {attribute, <<"name">>}])),
	?assertEqual(<<"jabber:iq:privacy">>, 
		exml:get_path(P, [{element, <<"query">>},{attribute, <<"xmlns">>}])),
	ok.
