-module(exmpp_client_bindig_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

announced_support_test() ->
	El = {xmlel, <<"features">>, [], 
		[{xmlel, <<"bind">>, [], []}]},
	?assert(exmpp_client_binding:announced_support(El)),
	El2 = {xmlel, <<"features">>, [], []},
	?assertNot(exmpp_client_binding:announced_support(El2)),
	El3 = {xmlel, <<"features">>, [], 
		[{xmlel, <<"bind">>, [], [{xmlel, <<"other">>, [], []}]}]},
	?assertThrow({resource_binding, announced_support, invalid_feature, _}, exmpp_client_binding:announced_support(El3)),
	ok.

bind_test() ->
	R = exmpp_client_binding:bind(<<"a">>),
	io:format("~p\n", [R]),
	?assertEqual(?NS_BIND, exxml:get_path(R, [{element, <<"bind">>}, {attribute, <<"xmlns">>}])),
	?assertEqual(<<"a">>, exxml:get_path(R, [{element, <<"bind">>}, {element, <<"resource">>}, cdata])),
	ok.

bounded_jid_test_disabled() ->
	%% TODO: for some reason this do not work well with cover.. disable for now
	exmpp:start(), %% argg.. needed for now for stringprep
	B = <<"<iq type='result' id='bind_2'>", 
	   "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>",
	  " <jid>somenode@example.com/someresource</jid> </bind> </iq>">>,
       {ok, [R]} = exxml:parse_document(B),
       ?assertMatch(<<"somenode@example.com/someresource">>, exmpp_jid:to_binary(exmpp_client_binding:bounded_jid(R))),
       ok.


