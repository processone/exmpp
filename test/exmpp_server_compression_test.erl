-module(exmpp_server_compression_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

feature_test() ->
	B = exmpp_server_compression:feature([<<"zlib">>, <<"lzw">>]),
	?assertMatch({xmlel, <<"compression">>, _, _}, B),
	?assertEqual(<<"http://jabber.org/features/compress">>, exxml:get_attribute(B, <<"xmlns">>)),
	?assertEqual(2, length([exxml:get_cdata(E) || E <- exxml:get_elements(B)])),
	?assert(lists:member(<<"zlib">>, [exxml:get_cdata(E) || E <- exxml:get_elements(B)])),
	?assert(lists:member(<<"lzw">>, [exxml:get_cdata(E) || E <- exxml:get_elements(B)])),
	ok.


failure_test() ->
	F = exmpp_server_compression:failure(<<"unsupported-method">>),
	?assertMatch({xmlel, <<"failure">>, _, _}, F),
	?assertEqual(<<"http://jabber.org/protocol/compress">>, exxml:get_attribute(F, <<"xmlns">>)),
	?assertMatch([{xmlel, <<"unsupported-method">>, _, _}], exxml:get_elements(F)),
	ok.

selected_method_test() ->
	S = <<" <compress xmlns='http://jabber.org/protocol/compress'>
	  	<method>zlib</method> </compress>">>,
	{ok, [C]} = exxml:parse_document(S),
	?assertEqual(<<"zlib">>, exmpp_server_compression:selected_method(C)),
	ok.

compressed_test() ->
	C = exmpp_server_compression:compressed(),
	?assertMatch({xmlel, <<"compressed">>, _, _}, C),
	?assertEqual(<<"http://jabber.org/protocol/compress">>, exxml:get_attribute(C, <<"xmlns">>)),
	ok.
