% $Id$

-module(jid).
-vsn('$Revision$').

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").
-include("internal/exmpp_xmpp.hrl").

-define(SETUP, fun()  -> exmpp:start(), error_logger:tty(false) end).
-define(CLEANUP, fun(_) -> application:stop(exmpp) end).

-define(NODE, "n").
-define(DOMAIN, "d").
-define(RESOURCE, "r").

-define(FJ1, #jid{
    node = <<"John">>,
    domain = <<"example.org">>,
    resource = <<"Work">>,
    lnode = <<"john">>,
    ldomain = <<"example.org">>,
    lresource = <<"Work">>
  }).
-define(FJ1_S, "John@example.org/Work").
-define(FJ1_B, <<"John@example.org/Work">>).
-define(FJ1_S_BAD1, "John" ++ [0] ++ "@example.org/Work").
-define(FJ1_S_BAD2, "John@example.org" ++ [128] ++ "/Work").
-define(FJ1_S_BAD3, "John@example.org/Work" ++ [0]).

-define(FJ2, #jid{
    node = undefined,
    domain = <<"example2.org">>,
    resource = <<"Work">>,
    lnode = undefined,
    ldomain = <<"example2.org">>,
    lresource = <<"Work">>
  }).
-define(FJ2_S, "example2.org/Work").
-define(FJ2_B, <<"example2.org/Work">>).
-define(FJ2_S_BAD1, "example2.org" ++ [128] ++ "/Work").
-define(FJ2_S_BAD2, "example2.org/Work" ++ [0]).

-define(BJ1, #jid{
    node = <<"John">>,
    domain = <<"example.org">>,
    resource = undefined,
    lnode = <<"john">>,
    ldomain = <<"example.org">>,
    lresource = undefined
  }).
-define(BJ1_S, "John@example.org").
-define(BJ1_B, <<"John@example.org">>).
-define(BJ1_S_BAD1, "John" ++ [0] ++ "@example.org").
-define(BJ1_S_BAD2, "John@example.org" ++ [128]).

-define(BJ2, #jid{
    node = undefined,
    domain = <<"example2.org">>,
    resource = undefined,
    lnode = undefined,
    ldomain = <<"example2.org">>,
    lresource = undefined
  }).
-define(BJ2_S, "example2.org").
-define(BJ2_B, <<"example2.org">>).
-define(BJ2_S_BAD1, "example2.org" ++ [128]).

-define(RES, "Work").
-define(RES_BAD, "Work" ++ [0]).

too_long_identifiers_test_() ->
    Node_TL = string:chars($n, 1024),
    Domain_TL = string:chars($d, 1024),
    Resource_TL = string:chars($r, 1024),
    JID_TL = Node_TL ++ [$@] ++ Domain_TL ++ [$/] ++ Resource_TL,
    Node_TL_B = list_to_binary(Node_TL),
    Domain_TL_B = list_to_binary(Domain_TL),
    Resource_TL_B = list_to_binary(Resource_TL),
    JID_TL_B = list_to_binary(JID_TL),
    Tests = [
      ?_assertThrow(
        {jid, make, too_long, {domain, Domain_TL}},
        exmpp_jid:make_jid(?NODE, Domain_TL, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, too_long, {node, Node_TL}},
        exmpp_jid:make_jid(Node_TL, ?DOMAIN, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, too_long, {resource, Resource_TL}},
        exmpp_jid:make_jid(?NODE, ?DOMAIN, Resource_TL)
      ),
      ?_assertThrow(
        {jid, parse, too_long, {jid, JID_TL}},
        exmpp_jid:parse_jid(JID_TL)
      ),
      ?_assertThrow(
        {jid, make, too_long, {domain, Domain_TL_B}},
        exmpp_jid:make_jid(list_to_binary(?NODE),
          Domain_TL_B, list_to_binary(?RESOURCE))
      ),
      ?_assertThrow(
        {jid, make, too_long, {node, Node_TL_B}},
        exmpp_jid:make_jid(Node_TL_B,
          list_to_binary(?DOMAIN), list_to_binary(?RESOURCE))
      ),
      ?_assertThrow(
        {jid, make, too_long, {resource, Resource_TL_B}},
        exmpp_jid:make_jid(list_to_binary(?NODE),
          list_to_binary(?DOMAIN), Resource_TL_B)
      ),
      ?_assertThrow(
        {jid, parse, too_long, {jid, JID_TL_B}},
        exmpp_jid:parse_jid(JID_TL_B)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_creation_with_bad_syntax_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, ""}},
        exmpp_jid:parse_jid("")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@"}},
        exmpp_jid:parse_jid("@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain"}},
        exmpp_jid:parse_jid("@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain@Domain"}},
        exmpp_jid:parse_jid("@Domain@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain/Resource"}},
        exmpp_jid:parse_jid("@Domain/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Node@"}},
        exmpp_jid:parse_jid("Node@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "Node@Domain@"}},
        exmpp_jid:parse_jid("Node@Domain@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "Node@@Domain"}},
        exmpp_jid:parse_jid("Node@@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Domain/"}},
        exmpp_jid:parse_jid("Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Node@Domain/"}},
        exmpp_jid:parse_jid("Node@Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@/"}},
        exmpp_jid:parse_jid("@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "Node@/"}},
        exmpp_jid:parse_jid("Node@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "Node@/Resource"}},
        exmpp_jid:parse_jid("Node@/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "/"}},
        exmpp_jid:parse_jid("/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "/Resource"}},
        exmpp_jid:parse_jid("/Resource")
      )
    ],
    TestsBinaryParsing =  [
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<>>}},
        exmpp_jid:parse_jid(<<>>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@">>}},
        exmpp_jid:parse_jid(<<"@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain">>}},
        exmpp_jid:parse_jid(<<"@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain@Domain">>}},
        exmpp_jid:parse_jid(<<"@Domain@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain/Resource">>}},
        exmpp_jid:parse_jid(<<"@Domain/Resource">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Node@">>}},
        exmpp_jid:parse_jid(<<"Node@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"Node@Domain@">>}},
        exmpp_jid:parse_jid(<<"Node@Domain@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"Node@@Domain">>}},
        exmpp_jid:parse_jid(<<"Node@@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Domain/">>}},
        exmpp_jid:parse_jid(<<"Domain/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Node@Domain/">>}},
        exmpp_jid:parse_jid(<<"Node@Domain/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@/">>}},
        exmpp_jid:parse_jid(<<"@/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"Node@/">>}},
        exmpp_jid:parse_jid(<<"Node@/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {jid, <<"Node@/Resource">>}},
        exmpp_jid:parse_jid(<<"Node@/Resource">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"/">>}},
        exmpp_jid:parse_jid(<<"/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"/Resource">>}},
        exmpp_jid:parse_jid(<<"/Resource">>)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests ++ TestsBinaryParsing}.

jid_creation_with_bad_chars_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?FJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?FJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?FJ1_S_BAD3)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?FJ2_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?FJ2_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?BJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?BJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(?BJ2_S_BAD1)
      )
    ],
    TestsBinaryParsing = [
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?FJ1_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?FJ1_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?FJ1_S_BAD3))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?FJ2_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?FJ2_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?BJ1_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?BJ1_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse_jid(list_to_binary(?BJ2_S_BAD1))
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests ++ TestsBinaryParsing}.

good_jid_creation_test_() ->
    Tests = [
      ?_assertMatch(?FJ1, exmpp_jid:parse_jid(?FJ1_S)),
      ?_assertMatch(?FJ2, exmpp_jid:parse_jid(?FJ2_S)),
      ?_assertMatch(?BJ1, exmpp_jid:parse_jid(?BJ1_S)),
      ?_assertMatch(?BJ2, exmpp_jid:parse_jid(?BJ2_S))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_stringification_test_() ->
    [
      ?_assertMatch(?FJ1_S, exmpp_jid:jid_to_list(?FJ1)),
      ?_assertMatch(?FJ2_S, exmpp_jid:jid_to_list(?FJ2)),
      ?_assertMatch(?BJ1_S, exmpp_jid:jid_to_list(?BJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:jid_to_list(?BJ2)),
      ?_assertMatch(?FJ1_B, exmpp_jid:jid_to_binary(?FJ1)),
      ?_assertMatch(?FJ2_B, exmpp_jid:jid_to_binary(?FJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:jid_to_binary(?BJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:jid_to_binary(?BJ2))
    ].

jid_arg_stringification_test_() ->
    [
      ?_assertMatch("d", exmpp_jid:jid_to_list(undefined, "d")),
      ?_assertMatch("n@d", exmpp_jid:jid_to_list("n", "d")),
      ?_assertMatch("n@d/r", exmpp_jid:jid_to_list("n", "d", "r")),
      ?_assertMatch(<<"d">>, exmpp_jid:jid_to_binary(undefined, "d")),
      ?_assertMatch(<<"n@d">>, exmpp_jid:jid_to_binary("n", "d")),
      ?_assertMatch(<<"n@d/r">>, exmpp_jid:jid_to_binary("n", "d", "r")),
      ?_assertMatch(<<"d">>, exmpp_jid:jid_to_binary(undefined, <<"d">>)),
      ?_assertMatch(<<"n@d">>, exmpp_jid:jid_to_binary(<<"n">>, <<"d">>)),
      ?_assertMatch(<<"n@d/r">>, exmpp_jid:jid_to_binary(<<"n">>, <<"d">>,
          <<"r">>))
    ].

bare_jid_stringification_test_() ->
    [
      ?_assertMatch(?BJ1_S, exmpp_jid:bare_jid_to_list(?FJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:bare_jid_to_list(?FJ2)),
      ?_assertMatch(?BJ1_S, exmpp_jid:bare_jid_to_list(?BJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:bare_jid_to_list(?BJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:bare_jid_to_binary(?FJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:bare_jid_to_binary(?FJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:bare_jid_to_binary(?BJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:bare_jid_to_binary(?BJ2))
    ].

jid_conversion_test_() ->
    [
      ?_assertMatch(?BJ1, exmpp_jid:jid_to_bare_jid(?FJ1)),
      ?_assertMatch(?BJ1, exmpp_jid:jid_to_bare_jid(?BJ1)),
      ?_assertMatch(?BJ2, exmpp_jid:jid_to_bare_jid(?FJ2)),
      ?_assertMatch(?BJ2, exmpp_jid:jid_to_bare_jid(?BJ2))
    ].

bare_jid_conversion_test_() ->
    Tests = [
      ?_assertMatch(?FJ1, exmpp_jid:bare_jid_to_jid(?FJ1, ?RES)),
      ?_assertMatch(?FJ1, exmpp_jid:bare_jid_to_jid(?BJ1, ?RES)),
      ?_assertMatch(?FJ2, exmpp_jid:bare_jid_to_jid(?FJ2, ?RES)),
      ?_assertMatch(?FJ2, exmpp_jid:bare_jid_to_jid(?BJ2, ?RES)),
      ?_assertMatch(?FJ1, exmpp_jid:bare_jid_to_jid(?FJ1, undefined)),
      ?_assertMatch(?BJ1, exmpp_jid:bare_jid_to_jid(?BJ1, undefined))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

bare_jid_conversion_with_bad_resource_test_() ->
    Resource_TL = string:chars($r, 1024),
    Tests = [
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:bare_jid_to_jid(?FJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:bare_jid_to_jid(?FJ1, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:bare_jid_to_jid(?BJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:bare_jid_to_jid(?BJ1, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:bare_jid_to_jid(?FJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:bare_jid_to_jid(?FJ2, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:bare_jid_to_jid(?BJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:bare_jid_to_jid(?BJ2, Resource_TL)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

accessors_test_() ->
    Tests = [
      ?_assertMatch(?BJ2_S, exmpp_jid:domain_as_list(?FJ2)),
      ?_assertMatch(?BJ2_S, exmpp_jid:ldomain_as_list(?FJ2)),
      ?_assertMatch(undefined, exmpp_jid:lnode_as_list(?FJ2)),
      ?_assertMatch(undefined, exmpp_jid:resource_as_list(?BJ1))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_comparison_test_() ->
    [
      ?_assert(exmpp_jid:compare_jids(?FJ1, ?FJ1)),
      ?_assertNot(exmpp_jid:compare_jids(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:compare_jids(?FJ2, ?FJ2)),
      ?_assertNot(exmpp_jid:compare_jids(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:compare_jids(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:compare_jids(?BJ1, ?BJ2))
    ].

bare_jid_comparison_test_() ->
    [
      ?_assert(exmpp_jid:compare_bare_jids(?FJ1, ?FJ1)),
      ?_assert(exmpp_jid:compare_bare_jids(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:compare_bare_jids(?FJ2, ?FJ2)),
      ?_assert(exmpp_jid:compare_bare_jids(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:compare_bare_jids(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:compare_bare_jids(?BJ1, ?BJ2))
    ].

domain_comparison_test_() ->
    [
      ?_assert(exmpp_jid:compare_domains(?FJ1, ?FJ1)),
      ?_assert(exmpp_jid:compare_domains(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:compare_domains(?FJ2, ?FJ2)),
      ?_assert(exmpp_jid:compare_domains(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:compare_domains(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:compare_domains(?FJ1, ?BJ2))
    ].
