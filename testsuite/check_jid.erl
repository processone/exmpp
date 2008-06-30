% $Id$

-module(check_jid).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    exmpp:start(),
    test_too_long_identifiers(),
    test_good_jid_creation(),
    test_jid_creation_with_bad_syntax(),
    test_jid_creation_with_bad_chars(),
    test_good_bare_jid_creation(),
    test_bare_jid_creation_with_bad_chars(),
    test_jid_stringification(),
    test_bare_jid_stringification(),
    test_jid_conversion(),
    test_bare_jid_conversion(),
    test_bare_jid_conversion_with_bad_resource(),
    test_jid_comparison(),
    test_bare_jid_comparison(),
    test_domain_comparison(),
    ok.

% --------------------------------------------------------------------
% JID handling testsuite.
% --------------------------------------------------------------------

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
-define(BJ2_S_BAD1, "example2.org" ++ [128]).

-define(RES, "Work").
-define(RES_BAD, "Work" ++ [0]).

test_too_long_identifiers() ->
    try
        exmpp_jid:make_jid(?NODE, ?DOMAIN_TOO_LONG, ?RESOURCE),
        testsuite:fail()
    catch
        throw:{jid, make, domain_too_long,
          {?NODE, _DOMAIN_TOO_LONG, undefined}} ->
            ok
    end,
    try
        exmpp_jid:make_jid(?NODE_TOO_LONG, ?DOMAIN, ?RESOURCE),
        testsuite:fail()
    catch
        throw:{jid, make, node_too_long,
          {_NODE_TOO_LONG, ?DOMAIN, undefined}} ->
            ok
    end,
    try
        exmpp_jid:make_jid(?NODE, ?DOMAIN, ?RESOURCE_TOO_LONG),
        testsuite:fail()
    catch
        throw:{jid, convert, resource_too_long,
          {?NODE, ?DOMAIN, _RESOURCE_TOO_LONG}} ->
            ok
    end,
    Jid1 = ?NODE_TOO_LONG ++ [$@] ++ ?DOMAIN_TOO_LONG,
    try
        exmpp_jid:string_to_bare_jid(Jid1),
        testsuite:fail()
    catch
        throw:{jid, parse, jid_too_long, {Jid1, undefined, undefined}} ->
            ok
    end,
    Jid2 = ?NODE_TOO_LONG ++ [$@] ++ ?DOMAIN_TOO_LONG ++ [$/] ++
      ?RESOURCE_TOO_LONG,
    try
        exmpp_jid:string_to_jid(Jid2),
        testsuite:fail()
    catch
        throw:{jid, parse, jid_too_long, {Jid2, undefined, undefined}} ->
            ok
    end,
    ok.

test_good_jid_creation() ->
    FJ1 = exmpp_jid:string_to_jid(?FJ1_S),
    testsuite:is(FJ1, ?FJ1),
    FJ2 = exmpp_jid:string_to_jid(?FJ2_S),
    testsuite:is(FJ2, ?FJ2),
    BJ1 = exmpp_jid:string_to_jid(?BJ1_S),
    testsuite:is(BJ1, ?BJ1),
    BJ2 = exmpp_jid:string_to_jid(?BJ2_S),
    testsuite:is(BJ2, ?BJ2),
    ok.

test_jid_creation_with_bad_syntax() ->
    try
        exmpp_jid:string_to_jid(""),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_end_of_string,
          {"", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("@"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"@", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("@Domain"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"@Domain", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("@Domain@Domain"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"@Domain@Domain", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("@Domain/Resource"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"@Domain/Resource", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_end_of_string,
          {"Node@", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@Domain@"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"Node@Domain@", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@@Domain"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"Node@@Domain", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Domain/"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_end_of_string,
          {"Domain/", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@Domain/"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_end_of_string,
          {"Node@Domain/", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("@/"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_node_separator,
          {"@/", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@/"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_resource_separator,
          {"Node@/", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("Node@/Resource"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_resource_separator,
          {"Node@/Resource", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("/"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_resource_separator,
          {"/", undefined, undefined}} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid("/Resource"),
        testsuite:fail()
    catch
        throw:{jid, parse, unexpected_resource_separator,
          {"/Resource", undefined, undefined}} ->
            ok
    end,
    ok.

test_jid_creation_with_bad_chars() ->
    try
        exmpp_jid:string_to_jid(?FJ1_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nodeprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?FJ1_S_BAD2),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?FJ1_S_BAD3),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?FJ2_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?FJ2_S_BAD2),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?BJ1_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nodeprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?BJ1_S_BAD2),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_jid(?BJ2_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    ok.

test_good_bare_jid_creation() ->
    FJ1 = exmpp_jid:string_to_bare_jid(?FJ1_S),
    testsuite:is(FJ1, ?BJ1),
    FJ2 = exmpp_jid:string_to_bare_jid(?FJ2_S),
    testsuite:is(FJ2, ?BJ2),
    BJ1 = exmpp_jid:string_to_bare_jid(?BJ1_S),
    testsuite:is(BJ1, ?BJ1),
    BJ2 = exmpp_jid:string_to_bare_jid(?BJ2_S),
    testsuite:is(BJ2, ?BJ2),
    ok.

test_bare_jid_creation_with_bad_chars() ->
    try
        exmpp_jid:string_to_bare_jid(?FJ1_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nodeprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_bare_jid(?FJ1_S_BAD2),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    FJ1_Bad3 = exmpp_jid:string_to_bare_jid(?FJ1_S_BAD3),
    testsuite:is(FJ1_Bad3, ?BJ1),
    try
        exmpp_jid:string_to_bare_jid(?FJ2_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    FJ2_Bad2 = exmpp_jid:string_to_bare_jid(?FJ2_S_BAD2),
    testsuite:is(FJ2_Bad2, ?BJ2),
    try
        exmpp_jid:string_to_bare_jid(?BJ1_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nodeprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_bare_jid(?BJ1_S_BAD2),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:string_to_bare_jid(?BJ2_S_BAD1),
        testsuite:fail()
    catch
        throw:{stringprep, nameprep, undefined, _} ->
            ok
    end,
    ok.

test_jid_stringification() ->
    FJ1 = exmpp_jid:jid_to_string(?FJ1),
    testsuite:is(FJ1, ?FJ1_S),
    FJ2 = exmpp_jid:jid_to_string(?FJ2),
    testsuite:is(FJ2, ?FJ2_S),
    BJ1 = exmpp_jid:jid_to_string(?BJ1),
    testsuite:is(BJ1, ?BJ1_S),
    BJ2 = exmpp_jid:jid_to_string(?BJ2),
    testsuite:is(BJ2, ?BJ2_S),
    ok.

test_bare_jid_stringification() ->
    FJ1 = exmpp_jid:bare_jid_to_string(?FJ1),
    testsuite:is(FJ1, ?BJ1_S),
    FJ2 = exmpp_jid:bare_jid_to_string(?FJ2),
    testsuite:is(FJ2, ?BJ2_S),
    BJ1 = exmpp_jid:bare_jid_to_string(?BJ1),
    testsuite:is(BJ1, ?BJ1_S),
    BJ2 = exmpp_jid:bare_jid_to_string(?BJ2),
    testsuite:is(BJ2, ?BJ2_S),
    ok.

test_jid_conversion() ->
    FJ1 = exmpp_jid:jid_to_bare_jid(?FJ1),
    testsuite:is(FJ1, ?BJ1),
    BJ1 = exmpp_jid:jid_to_bare_jid(?BJ1),
    testsuite:is(BJ1, ?BJ1),
    FJ2 = exmpp_jid:jid_to_bare_jid(?FJ2),
    testsuite:is(FJ2, ?BJ2),
    BJ2 = exmpp_jid:jid_to_bare_jid(?BJ2),
    testsuite:is(BJ2, ?BJ2),
    ok.

test_bare_jid_conversion() ->
    FJ1 = exmpp_jid:bare_jid_to_jid(?FJ1, ?RES),
    testsuite:is(FJ1, ?FJ1),
    BJ1 = exmpp_jid:bare_jid_to_jid(?BJ1, ?RES),
    testsuite:is(BJ1, ?FJ1),
    FJ2 = exmpp_jid:bare_jid_to_jid(?FJ2, ?RES),
    testsuite:is(FJ2, ?FJ2),
    BJ2 = exmpp_jid:bare_jid_to_jid(?BJ2, ?RES),
    testsuite:is(BJ2, ?FJ2),
    testsuite:is(exmpp_jid:bare_jid_to_jid(?FJ1, undefined), ?FJ1),
    testsuite:is(exmpp_jid:bare_jid_to_jid(?BJ1, undefined), ?BJ1),
    ok.

test_bare_jid_conversion_with_bad_resource() ->
    try
        exmpp_jid:bare_jid_to_jid(?FJ1, ?RES_BAD),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?FJ1, ?RESOURCE_TOO_LONG),
        testsuite:fail()
    catch
        throw:{jid, convert, resource_too_long, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?BJ1, ?RES_BAD),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?BJ1, ?RESOURCE_TOO_LONG),
        testsuite:fail()
    catch
        throw:{jid, convert, resource_too_long, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?FJ2, ?RES_BAD),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?FJ2, ?RESOURCE_TOO_LONG),
        testsuite:fail()
    catch
        throw:{jid, convert, resource_too_long, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?BJ2, ?RES_BAD),
        testsuite:fail()
    catch
        throw:{stringprep, resourceprep, undefined, _} ->
            ok
    end,
    try
        exmpp_jid:bare_jid_to_jid(?BJ2, ?RESOURCE_TOO_LONG),
        testsuite:fail()
    catch
        throw:{jid, convert, resource_too_long, _} ->
            ok
    end,
    ok.

test_jid_comparison() ->
    testsuite:is(exmpp_jid:compare_jids(?FJ1, ?FJ1), true),
    testsuite:is(exmpp_jid:compare_jids(?FJ1, ?BJ1), false),
    testsuite:is(exmpp_jid:compare_jids(?FJ2, ?FJ2), true),
    testsuite:is(exmpp_jid:compare_jids(?FJ2, ?BJ2), false),
    testsuite:is(exmpp_jid:compare_jids(?FJ1, ?FJ2), false),
    testsuite:is(exmpp_jid:compare_jids(?BJ1, ?BJ2), false),
    ok.

test_bare_jid_comparison() ->
    testsuite:is(exmpp_jid:compare_bare_jids(?FJ1, ?FJ1), true),
    testsuite:is(exmpp_jid:compare_bare_jids(?FJ1, ?BJ1), true),
    testsuite:is(exmpp_jid:compare_bare_jids(?FJ2, ?FJ2), true),
    testsuite:is(exmpp_jid:compare_bare_jids(?FJ2, ?BJ2), true),
    testsuite:is(exmpp_jid:compare_bare_jids(?FJ1, ?FJ2), false),
    testsuite:is(exmpp_jid:compare_bare_jids(?BJ1, ?BJ2), false),
    ok.

test_domain_comparison() ->
    testsuite:is(exmpp_jid:compare_domains(?FJ1, ?FJ1), true),
    testsuite:is(exmpp_jid:compare_domains(?FJ1, ?BJ1), true),
    testsuite:is(exmpp_jid:compare_domains(?FJ2, ?FJ2), true),
    testsuite:is(exmpp_jid:compare_domains(?FJ2, ?BJ2), true),
    testsuite:is(exmpp_jid:compare_domains(?FJ1, ?FJ2), false),
    testsuite:is(exmpp_jid:compare_domains(?FJ1, ?BJ2), false),
    ok.
