-module(exmpp_client_compression_test).

-include_lib("eunit/include/eunit.hrl").


-include("exmpp.hrl").

announced_methods_test() ->
   S = <<"<stream:features> <starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>",
     " <compression xmlns='http://jabber.org/features/compress'>",
     "<method>zlib</method>",
    "<method>lzw</method>",
  "</compression></stream:features>">>,
  {ok, [F]} = exml:parse_document(S),
  ?assert(lists:member(<<"zlib">>, exmpp_client_compression:announced_methods(F))),
  ?assert(lists:member(<<"lzw">>, exmpp_client_compression:announced_methods(F))),
  ?assertEqual(2, length(exmpp_client_compression:announced_methods(F))),
  ok.

selected_method_test() ->
	R = exmpp_client_compression:selected_method(<<"zlib">>),
	?assertEqual(<<"zlib">>, exml:get_path(R, [{element, <<"method">>}, cdata])),
	?assertEqual(?NS_COMPRESS, exml:get_path(R, [{attribute, <<"xmlns">>}])),
	ok.


