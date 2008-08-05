% $Id$

-module(jid).
-vsn('$Revision$').

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

-define(SETUP, fun()  -> exmpp:start(), error_logger:tty(false) end).
-define(CLEANUP, fun(_) -> application:stop(exmpp) end).

-define(NODE, "n").
-define(DOMAIN, "d").
-define(RESOURCE, "r").

-define(NODE_TOO_LONG, "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn").
-define(DOMAIN_TOO_LONG, "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd").
-define(RESOURCE_TOO_LONG, "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr").

-define(FJ1, #jid{
    node = "John",
    domain = "example.org",
    resource = "Work",
    lnode = "john",
    ldomain = "example.org",
    lresource = "Work"
  }).
-define(FJ1_S, "John@example.org/Work").
-define(FJ1_B, <<"John@example.org/Work">>).
-define(FJ1_S_BAD1, "John" ++ [0] ++ "@example.org/Work").
-define(FJ1_S_BAD2, "John@example.org" ++ [128] ++ "/Work").
-define(FJ1_S_BAD3, "John@example.org/Work" ++ [0]).

-define(FJ2, #jid{
    node = undefined,
    domain = "example2.org",
    resource = "Work",
    lnode = undefined,
    ldomain = "example2.org",
    lresource = "Work"
  }).
-define(FJ2_S, "example2.org/Work").
-define(FJ2_B, <<"example2.org/Work">>).
-define(FJ2_S_BAD1, "example2.org" ++ [128] ++ "/Work").
-define(FJ2_S_BAD2, "example2.org/Work" ++ [0]).

-define(BJ1, #jid{
    node = "John",
    domain = "example.org",
    resource = undefined,
    lnode = "john",
    ldomain = "example.org",
    lresource = undefined
  }).
-define(BJ1_S, "John@example.org").
-define(BJ1_B, <<"John@example.org">>).
-define(BJ1_S_BAD1, "John" ++ [0] ++ "@example.org").
-define(BJ1_S_BAD2, "John@example.org" ++ [128]).

-define(BJ2, #jid{
    node = undefined,
    domain = "example2.org",
    resource = undefined,
    lnode = undefined,
    ldomain = "example2.org",
    lresource = undefined
  }).
-define(BJ2_S, "example2.org").
-define(BJ2_B, <<"example2.org">>).
-define(BJ2_S_BAD1, "example2.org" ++ [128]).

-define(RES, "Work").
-define(RES_BAD, "Work" ++ [0]).

too_long_identifiers_test_() ->
    Too_Long_JID1 = ?NODE_TOO_LONG ++ [$@] ++ ?DOMAIN_TOO_LONG,
    Too_Long_JID2 = ?NODE_TOO_LONG ++ [$@] ++ ?DOMAIN_TOO_LONG ++ [$/] ++
        ?RESOURCE_TOO_LONG,
    Tests = [
      ?_assertThrow(
        {jid, make, domain_too_long, {?NODE, _DOMAIN_TOO_LONG, undefined}},
        exmpp_jid:make_jid(?NODE, ?DOMAIN_TOO_LONG, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, node_too_long, {_NODE_TOO_LONG, ?DOMAIN, undefined}},
        exmpp_jid:make_jid(?NODE_TOO_LONG, ?DOMAIN, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, resource_too_long, {?NODE, ?DOMAIN, _RESOURCE_TOO_LONG}},
        exmpp_jid:make_jid(?NODE, ?DOMAIN, ?RESOURCE_TOO_LONG)
      ),
      ?_assertThrow(
        {jid, parse, jid_too_long, {Too_Long_JID1, undefined, undefined}},
        exmpp_jid:list_to_bare_jid(Too_Long_JID1)
      ),
      ?_assertThrow(
        {jid, parse, jid_too_long, {Too_Long_JID2, undefined, undefined}},
        exmpp_jid:list_to_jid(Too_Long_JID2)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_creation_with_bad_syntax_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string,
          {"", undefined, undefined}},
        exmpp_jid:list_to_jid("")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"@", undefined, undefined}},
        exmpp_jid:list_to_jid("@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"@Domain", undefined, undefined}},
        exmpp_jid:list_to_jid("@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"@Domain@Domain", undefined, undefined}},
        exmpp_jid:list_to_jid("@Domain@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"@Domain/Resource", undefined, undefined}},
        exmpp_jid:list_to_jid("@Domain/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string,
          {"Node@", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"Node@Domain@", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@Domain@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"Node@@Domain", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string,
          {"Domain/", undefined, undefined}},
        exmpp_jid:list_to_jid("Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string,
          {"Node@Domain/", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator,
          {"@/", undefined, undefined}},
        exmpp_jid:list_to_jid("@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {"Node@/", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {"Node@/Resource", undefined, undefined}},
        exmpp_jid:list_to_jid("Node@/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {"/", undefined, undefined}},
        exmpp_jid:list_to_jid("/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {"/Resource", undefined, undefined}},
        exmpp_jid:list_to_jid("/Resource")
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_creation_with_bad_chars_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, make, invalid_node, _},
        exmpp_jid:list_to_jid(?FJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_jid(?FJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid_resource, _},
        exmpp_jid:list_to_jid(?FJ1_S_BAD3)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_jid(?FJ2_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid_resource, _},
        exmpp_jid:list_to_jid(?FJ2_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid_node, _},
        exmpp_jid:list_to_jid(?BJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_jid(?BJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_jid(?BJ2_S_BAD1)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

good_jid_creation_test_() ->
    Tests = [
      ?_assertMatch(?FJ1, exmpp_jid:list_to_jid(?FJ1_S)),
      ?_assertMatch(?FJ2, exmpp_jid:list_to_jid(?FJ2_S)),
      ?_assertMatch(?BJ1, exmpp_jid:list_to_jid(?BJ1_S)),
      ?_assertMatch(?BJ2, exmpp_jid:list_to_jid(?BJ2_S))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

bare_jid_creation_with_bad_chars_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, make, invalid_node, _},
        exmpp_jid:list_to_bare_jid(?FJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_bare_jid(?FJ1_S_BAD2)
      ),
      ?_assertMatch(?BJ1, exmpp_jid:list_to_bare_jid(?FJ1_S_BAD3)),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_bare_jid(?FJ2_S_BAD1)
      ),
      ?_assertMatch(?BJ2, exmpp_jid:list_to_bare_jid(?FJ2_S_BAD2)),
      ?_assertThrow(
        {jid, make, invalid_node, _},
        exmpp_jid:list_to_bare_jid(?BJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_bare_jid(?BJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid_domain, _},
        exmpp_jid:list_to_bare_jid(?BJ2_S_BAD1)
      )
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
    Tests = [
      ?_assertThrow(
        {jid, convert, invalid_resource, _},
        exmpp_jid:bare_jid_to_jid(?FJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, resource_too_long, _},
        exmpp_jid:bare_jid_to_jid(?FJ1, ?RESOURCE_TOO_LONG)
      ),
      ?_assertThrow(
        {jid, convert, invalid_resource, _},
        exmpp_jid:bare_jid_to_jid(?BJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, resource_too_long, _},
        exmpp_jid:bare_jid_to_jid(?BJ1, ?RESOURCE_TOO_LONG)
      ),
      ?_assertThrow(
        {jid, convert, invalid_resource, _},
        exmpp_jid:bare_jid_to_jid(?FJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, resource_too_long, _},
        exmpp_jid:bare_jid_to_jid(?FJ2, ?RESOURCE_TOO_LONG)
      ),
      ?_assertThrow(
        {jid, convert, invalid_resource, _},
        exmpp_jid:bare_jid_to_jid(?BJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, resource_too_long, _},
        exmpp_jid:bare_jid_to_jid(?BJ2, ?RESOURCE_TOO_LONG)
      )
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
