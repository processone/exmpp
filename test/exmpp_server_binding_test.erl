-module(exmpp_server_binding_test).

-include_lib("eunit/include/eunit.hrl").


-include("exmpp.hrl").


feature_test() ->
	B = exmpp_server_binding:feature(),
	?assertMatch({xmlel, <<"bind">>, _, _}, B),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-bind">>, exxml:get_attribute(B, <<"xmlns">>)),
	ok.

wished_resource_test() ->
	{ok, [Bind1]} = exxml:parse_document(<<"<iq type='set' id='bind_2'>
		  <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
		    <resource>someresource</resource>
		  </bind>
		  </iq>">>),
	?assertEqual(<<"someresource">>, exmpp_server_binding:wished_resource(Bind1)),
	{ok, [Bind2]} = exxml:parse_document(<<"<iq type='set' id='bind_2'>
		  <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/> </iq>">>),
	?assertEqual(undefined, exmpp_server_binding:wished_resource(Bind2)),
	ok.

bind_test_disabled() ->
	exmpp:start(), %%needed for now.. for stringprep.  Not working from eunit..
	{ok, [Bind]} = exxml:parse_document(<<"<iq type='set' id='bind_2'>
		  <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
		    <resource>someresource</resource>
		  </bind>
		  </iq>">>),
		  BindResponse = exmpp_server_binding:bind(Bind, exmpp_jid:make(<<"resultjid@domain.com">>)),
		  ?assertEqual(<<"resultjid@domain.com">>, 
			  exxml:get_path(BindResponse, [{element, bind}, {element, jid}, cdata])),
		  ?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-bind">>, 
			  exxml:get_path(BindResponse, [{element, bind}, {attribute, <<"xmlns">>}])),
		  ok.


error_test() ->
	{ok, [Bind]} = exxml:parse_document(<<"<iq type='set' id='bind_2'>
		  <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
		    <resource>someresource</resource>
		  </bind>
		  </iq>">>),
	E = exmpp_server_binding:error(Bind, <<"forbidden">>),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>, 
		exxml:get_path(E, [{element, <<"error">>}, {element, <<"forbidden">>}, {attribute, <<"xmlns">>}])),
	ok.
