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
        test_jid_creation(),
        test_bare_jid_creation(),
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
-define(BJ1, #jid{
  user = "John",
  server = "example.org",
  resource = "",
  luser = "john",
  lserver = "example.org",
  lresource = ""
}).
-define(BJ1_S, "John@example.org").
-define(U1, "John").
-define(S1, "example.org").
-define(R1, "Work").

test_jid_creation() ->
    FJ1 = exmpp_jid:string_to_jid(?FJ1_S),
    testsuite:is(FJ1, ?FJ1),
    BJ1 = exmpp_jid:string_to_jid(?BJ1_S),
    testsuite:is(BJ1, ?BJ1),
    ok.

test_bare_jid_creation() ->
    FJ1 = exmpp_jid:string_to_bare_jid(?FJ1_S),
    testsuite:is(FJ1, ?BJ1),
    BJ1 = exmpp_jid:string_to_bare_jid(?BJ1_S),
    testsuite:is(BJ1, ?BJ1),
    ok.
