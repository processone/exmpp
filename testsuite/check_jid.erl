% $Id$

-module(check_jid).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	exmpp:start(),
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
	ok.

% --------------------------------------------------------------------
% JID handling testsuite.
% --------------------------------------------------------------------

-define(FJ1, #jid{
  user = "John",
  server = "example.org",
  resource = "Work",
  luser = "john",
  lserver = "example.org",
  lresource = "Work"
}).
-define(FJ1_S, "John@example.org/Work").
-define(FJ1_S_Bad1, "John" ++ [0] ++ "@example.org/Work").
-define(FJ1_S_Bad2, "John@example.org" ++ [128] ++ "/Work").
-define(FJ1_S_Bad3, "John@example.org/Work" ++ [0]).

-define(FJ2, #jid{
  user = undefined,
  server = "example.org",
  resource = "Work",
  luser = undefined,
  lserver = "example.org",
  lresource = "Work"
}).
-define(FJ2_S, "example.org/Work").
-define(FJ2_S_Bad1, "example.org" ++ [128] ++ "/Work").
-define(FJ2_S_Bad2, "example.org/Work" ++ [0]).

-define(BJ1, #jid{
  user = "John",
  server = "example.org",
  resource = undefined,
  luser = "john",
  lserver = "example.org",
  lresource = undefined
}).
-define(BJ1_S, "John@example.org").
-define(BJ1_S_Bad1, "John" ++ [0] ++ "@example.org").
-define(BJ1_S_Bad2, "John@example.org" ++ [128]).

-define(BJ2, #jid{
  user = undefined,
  server = "example.org",
  resource = undefined,
  luser = undefined,
  lserver = "example.org",
  lresource = undefined
}).
-define(BJ2_S, "example.org").
-define(BJ2_S_Bad1, "example.org" ++ [128]).

-define(Res, "Work").
-define(Res_Bad, "Work" ++ [0]).

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
    testsuite:is(exmpp_jid:string_to_jid(""),
      {error, unexpected_end_of_string}),
    testsuite:is(exmpp_jid:string_to_jid("@"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("@Domain"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("@Domain@Domain"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("@Domain/Resource"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("Node@"),
      {error, unexpected_end_of_string}),
    testsuite:is(exmpp_jid:string_to_jid("Node@Domain@"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("Node@@Domain"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("Domain/"),
      {error, unexpected_end_of_string}),
    testsuite:is(exmpp_jid:string_to_jid("Node@Domain/"),
      {error, unexpected_end_of_string}),
    testsuite:is(exmpp_jid:string_to_jid("@/"),
      {error, unexpected_node_separator}),
    testsuite:is(exmpp_jid:string_to_jid("Node@/"),
      {error, unexpected_resource_separator}),
    testsuite:is(exmpp_jid:string_to_jid("Node@/Resource"),
      {error, unexpected_resource_separator}),
    testsuite:is(exmpp_jid:string_to_jid("/"),
      {error, unexpected_resource_separator}),
    testsuite:is(exmpp_jid:string_to_jid("/Resource"),
      {error, unexpected_resource_separator}),
    ok.

test_jid_creation_with_bad_chars() ->
    FJ1_Bad1 = exmpp_jid:string_to_jid(?FJ1_S_Bad1),
    testsuite:is(FJ1_Bad1, {error, bad_user}),
    FJ1_Bad2 = exmpp_jid:string_to_jid(?FJ1_S_Bad2),
    testsuite:is(FJ1_Bad2, {error, bad_server}),
    FJ1_Bad3 = exmpp_jid:string_to_jid(?FJ1_S_Bad3),
    testsuite:is(FJ1_Bad3, {error, bad_resource}),
    FJ2_Bad1 = exmpp_jid:string_to_jid(?FJ2_S_Bad1),
    testsuite:is(FJ2_Bad1, {error, bad_server}),
    FJ2_Bad2 = exmpp_jid:string_to_jid(?FJ2_S_Bad2),
    testsuite:is(FJ2_Bad2, {error, bad_resource}),
    BJ1_Bad1 = exmpp_jid:string_to_jid(?BJ1_S_Bad1),
    testsuite:is(BJ1_Bad1, {error, bad_user}),
    BJ1_Bad2 = exmpp_jid:string_to_jid(?BJ1_S_Bad2),
    testsuite:is(BJ1_Bad2, {error, bad_server}),
    BJ2_Bad1 = exmpp_jid:string_to_jid(?BJ2_S_Bad1),
    testsuite:is(BJ2_Bad1, {error, bad_server}),
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
    FJ1_Bad1 = exmpp_jid:string_to_bare_jid(?FJ1_S_Bad1),
    testsuite:is(FJ1_Bad1, {error, bad_user}),
    FJ1_Bad2 = exmpp_jid:string_to_bare_jid(?FJ1_S_Bad2),
    testsuite:is(FJ1_Bad2, {error, bad_server}),
    FJ1_Bad3 = exmpp_jid:string_to_bare_jid(?FJ1_S_Bad3),
    testsuite:is(FJ1_Bad3, ?BJ1),
    FJ2_Bad1 = exmpp_jid:string_to_bare_jid(?FJ2_S_Bad1),
    testsuite:is(FJ2_Bad1, {error, bad_server}),
    FJ2_Bad2 = exmpp_jid:string_to_bare_jid(?FJ2_S_Bad2),
    testsuite:is(FJ2_Bad2, ?BJ2),
    BJ1_Bad1 = exmpp_jid:string_to_bare_jid(?BJ1_S_Bad1),
    testsuite:is(BJ1_Bad1, {error, bad_user}),
    BJ1_Bad2 = exmpp_jid:string_to_bare_jid(?BJ1_S_Bad2),
    testsuite:is(BJ1_Bad2, {error, bad_server}),
    BJ2_Bad1 = exmpp_jid:string_to_bare_jid(?BJ2_S_Bad1),
    testsuite:is(BJ2_Bad1, {error, bad_server}),
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
    FJ1 = exmpp_jid:bare_jid_to_jid(?FJ1, ?Res),
    testsuite:is(FJ1, ?FJ1),
    BJ1 = exmpp_jid:bare_jid_to_jid(?BJ1, ?Res),
    testsuite:is(BJ1, ?FJ1),
    FJ2 = exmpp_jid:bare_jid_to_jid(?FJ2, ?Res),
    testsuite:is(FJ2, ?FJ2),
    BJ2 = exmpp_jid:bare_jid_to_jid(?BJ2, ?Res),
    testsuite:is(BJ2, ?FJ2),
    ok.

test_bare_jid_conversion_with_bad_resource() ->
    FJ1 = exmpp_jid:bare_jid_to_jid(?FJ1, ?Res_Bad),
    testsuite:is(FJ1, {error, bad_resource}),
    BJ1 = exmpp_jid:bare_jid_to_jid(?BJ1, ?Res_Bad),
    testsuite:is(BJ1, {error, bad_resource}),
    FJ2 = exmpp_jid:bare_jid_to_jid(?FJ2, ?Res_Bad),
    testsuite:is(FJ2, {error, bad_resource}),
    BJ2 = exmpp_jid:bare_jid_to_jid(?BJ2, ?Res_Bad),
    testsuite:is(BJ2, {error, bad_resource}),
    ok.
