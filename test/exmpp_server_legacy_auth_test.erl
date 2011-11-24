-module(exmpp_server_legacy_auth_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


fields_test() ->
	S = <<"<iq type='get' to='shakespeare.lit' id='auth1'>
		  <query xmlns='jabber:iq:auth'/>
		</iq>">>,
	{ok, [Req]} = exml:parse_document(S),
	?assertMatch([{xmlel, <<"username">>, _, _}, {xmlel, <<"password">>, _, _}, {xmlel, <<"resource">>, _, _}],
		exml:get_elements(exml:get_element(exmpp_server_legacy_auth:fields(Req, plain),<<"query">>))),
	?assertMatch([{xmlel, <<"username">>, _, _}, {xmlel, <<"digest">>, _, _}, {xmlel, <<"resource">>, _, _}],
		exml:get_elements(exml:get_element(exmpp_server_legacy_auth:fields(Req, digest),<<"query">>))),
	?assertMatch([{xmlel, <<"username">>, _, _}, {xmlel, <<"password">>, _, _},{xmlel, <<"digest">>, _, _}, {xmlel, <<"resource">>, _, _}],
		exml:get_elements(exml:get_element(exmpp_server_legacy_auth:fields(Req, both),<<"query">>))),
	ok.
   
success_test() ->
	S = <<"<iq type='set' id='auth2'> <query xmlns='jabber:iq:auth'>
    	<username>bill</username> <password>Calli0pe</password>
    	<resource>globe</resource> </query> </iq>">>,
    	{ok, [Req]} = exml:parse_document(S),
	Sucess = exmpp_server_legacy_auth:success(Req),
	?assertMatch({xmlel, <<"iq">>, _, []}, Sucess),
	?assertEqual(<<"result">>, exml:get_attribute(Sucess, <<"type">>)),
	?assertEqual(<<"auth2">>, exml:get_attribute(Sucess, <<"id">>)),
	ok.

failure_test() ->
	S = <<"<iq type='set' id='auth2'> <query xmlns='jabber:iq:auth'>
    	<username>bill</username> <password>Calli0pe</password>
    	<resource>globe</resource> </query> </iq>">>,
    	{ok, [Req]} = exml:parse_document(S),
	F = exmpp_server_legacy_auth:failure(Req, <<"not-authorized">>),
	?assertMatch({xmlel, <<"iq">>, _, _}, F),
	?assertEqual(<<"401">>, 
		exml:get_path(F,[{element, <<"error">>}, {attribute, <<"code">>}])),
	?assertEqual(<<"auth">>, 
		exml:get_path(F,[{element, <<"error">>}, {attribute, <<"type">>}])),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>, 
		exml:get_path(F,[{element, <<"error">>}, {element, <<"not-authorized">>}, {attribute, <<"xmlns">>}])),
	ok.

want_fields_test() ->
	S = <<"<iq type='get' to='shakespeare.lit' id='auth1'>
		  <query xmlns='jabber:iq:auth'/>
		</iq>">>,
	{ok, [Req]} = exml:parse_document(S),
	?assert(exmpp_server_legacy_auth:want_fields(Req)),
	ok.


