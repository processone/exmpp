-module(exmpp_client_session_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


establish_test() ->
	R = exmpp_client_session:establish(),
	?assertEqual(<<"set">>, exxml:get_attr(R, <<"type">>)),
	?assertEqual(?NS_SESSION, exxml:get_path(R, [{element, <<"session">>}, {attribute, <<"xmlns">>}])),
	ok.


