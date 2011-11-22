-module(exmpp_xmlstream_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


start_test() ->
	{ok, P} = exml:start_parser([{root_depth, 1}]),
	X = exmpp_xmlstream:start(self(), P),
	?assertEqual(P, exmpp_xmlstream:get_parser(X)),
	exmpp_xmlstream:parse(X, <<"<stream><a>aa</a><b b='b'/>">>),
	receive
		#xmlstreamstart{element = E1} -> 
			?assertEqual({xmlel,<<"stream">>, [],undefined}, E1)
	after 0 ->
			?assert(false)
	end,
	receive
		#xmlstreamelement{element = E2} -> 
			?assertMatch({xmlel,<<"a">>, [],_}, E2),
			?assertEqual(<<"aa">>, exml:get_cdata(E2))
	after 0 ->
			?assert(false)
	end,
	receive
		#xmlstreamelement{element = E3} -> 
			?assertMatch({xmlel,<<"b">>, _,[]}, E3),
			?assertEqual(<<"b">>, exml:get_attribute(E3, <<"b">>))
	after 0 ->
			?assert(false)
	end,
	X2 = exmpp_xmlstream:reset(X),
	?assertEqual(P, exmpp_xmlstream:get_parser(X2)),
	exmpp_xmlstream:parse(X2, <<"<stream><a>aa</a><b b='b'/>">>),
	receive
		#xmlstreamstart{element = E21} -> 
			?assertEqual({xmlel,<<"stream">>, [],undefined}, E21)
	after 0 ->
			?assert(false)
	end,
	receive
		#xmlstreamelement{element = E22} -> 
			?assertMatch({xmlel,<<"a">>, [],_}, E22),
			?assertEqual(<<"aa">>, exml:get_cdata(E22))
	after 0 ->
			?assert(false)
	end,
	receive
		#xmlstreamelement{element = E23} -> 
			?assertMatch({xmlel,<<"b">>, _,[]}, E23),
			?assertEqual(<<"b">>, exml:get_attribute(E23, <<"b">>))
	after 0 ->
			?assert(false)
	end.


change_callback_test() ->
	{ok, P} = exml:start_parser([{root_depth, 1}]),
	X = exmpp_xmlstream:start(self(), P),
	X2 = exmpp_xmlstream:change_callback(X, no_callback),
	{ok, _, [A,B,C]} = exmpp_xmlstream:parse(X2, <<"<stream><a>aa</a><b b='b'/>">>),
	?assertEqual(#xmlstreamstart{element={xmlel,<<"stream">>, [],undefined}}, A),
	?assertMatch(#xmlstreamelement{element = {xmlel,<<"a">>, [],_}}, B),
	?assertMatch(#xmlstreamelement{element = {xmlel,<<"b">>, _,_}}, C),
	ok.

parse_element_test() ->
	R = exmpp_xmlstream:parse_element(<<"<a>hello</a>">>, [{root_depth, 0}]),
	?assertEqual({xmlel, <<"a">>, [], [{cdata, <<"hello">>}]}, R),
	ok.


% TODO: not sure how this is supposed to work. Review latter when working on BOSH that 

wrapper_tagnames_test() ->
	{ok, P} = exml:start_parser(),
	X = exmpp_xmlstream:start(no_callback, P),
	X2 = exmpp_xmlstream:set_wrapper_tagnames(X, [<<"body">>]),

	{ok, _, [A,B]} = exmpp_xmlstream:parse(X2, <<"<body><a>aa</a><b b='b'/></body>">>),
	?assertMatch(#xmlstreamelement{element = {xmlel,<<"a">>, [],_}}, A),
	?assertMatch(#xmlstreamelement{element = {xmlel,<<"b">>, _,_}}, B),
	ok.
