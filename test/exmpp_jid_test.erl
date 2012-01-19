-module(exmpp_jid_test).

-include_lib("eunit/include/eunit.hrl").

to_lower_test() ->
	?assertMatch({<<"a">>, <<"b">>, <<"c">>}, exmpp_jid:to_lower(<<"a@b/c">>)),
	ok.

make1_test() ->
	?assertThrow({jid, make, invalid, {node, <<"a@a">>}}, exmpp_jid:make({<<"a@a">>, <<"b">>, <<"c">>})),
	?assertMatch(<<"a">>, exmpp_jid:node(exmpp_jid:make({<<"a">>, <<"b">>, <<"c">>}))),
	?assertMatch(<<"b">>, exmpp_jid:domain(exmpp_jid:make({<<"a">>, <<"b">>, <<"c">>}))),
	?assertMatch(<<"c">>, exmpp_jid:resource(exmpp_jid:make({<<"a">>, <<"b">>, <<"c">>}))),
	ok.

to_binary_test() ->
	?assertMatch(<<"a@b/c">>, exmpp_jid:to_binary(exmpp_jid:parse(<<"a@b/c">>))),
	?assertMatch(<<"a@b">>, exmpp_jid:bare_to_binary(exmpp_jid:parse(<<"a@b/c">>))),
	ok.

parse_bad_test() ->
	?assertThrow({jid, parse, _, _}, exmpp_jid:to_binary(exmpp_jid:parse(<<"a@c@b/c">>))),
	ok.

