% $Id$

-module(check_legacy_auth).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_legacy_auth_request(),
	test_legacy_auth_fields(),
	test_legacy_auth_password(),
	test_legacy_auth_success(),
	test_legacy_auth_failure(),
	ok.

% --------------------------------------------------------------------
% Stream factory testsuite.
% --------------------------------------------------------------------

-define(REQUEST1,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "get"},
	{xmlattr, undefined, undefined, 'to', "dest"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_AUTH, undefined, 'query', [], []}
]}
).

-define(FIELDS1,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "result"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_AUTH, undefined, 'query', [], [
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'username',
		    [], []},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'password',
		    [], []},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'digest',
		    [], []},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'resource',
		    [], []}
	]}
]}
).

-define(PASSWORD1,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "set"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_AUTH, undefined, 'query', [], [
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'username',
		    [], [{xmlcdata, <<"User">>}]},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'password',
		    [], [{xmlcdata, <<"Password">>}]},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'resource',
		    [], [{xmlcdata, <<"Resource">>}]}
	]}
]}
).

-define(PASSWORD2,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "set"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_AUTH, undefined, 'query', [], [
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'username',
		    [], [{xmlcdata, <<"User">>}]},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'digest',
		    [], [{xmlcdata, <<"ab8bb63d7fb73e5b06b325ec1c147945cfac5a77">>}]},
		{xmlnselement, ?NS_JABBER_AUTH, undefined, 'resource',
		    [], [{xmlcdata, <<"Resource">>}]}
	]}
]}
).

-define(SUCCESS1,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "result"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], []}
).

-define(FAILURE1,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "result"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'error', [
		{xmlattr, undefined, undefined, 'code', "401"},
		{xmlattr, undefined, undefined, 'type', "auth"}
	], [
		{xmlnselement, ?NS_XMPP_STANZAS, undefined, 'not-authorized',
		    [], []}
	]}
]}
).

-define(FAILURE2,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "result"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'error', [
		{xmlattr, undefined, undefined, 'code', "409"},
		{xmlattr, undefined, undefined, 'type', "cancel"}
	], [
		{xmlnselement, ?NS_XMPP_STANZAS, undefined, 'conflict',
		    [], []}
	]}
]}
).

-define(FAILURE3,
{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'iq', [
	{xmlattr, undefined, undefined, 'type', "result"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], [
	{xmlnselement, ?NS_JABBER_CLIENT, undefined, 'error', [
		{xmlattr, undefined, undefined, 'code', "406"},
		{xmlattr, undefined, undefined, 'type', "modify"}
	], [
		{xmlnselement, ?NS_XMPP_STANZAS, undefined, 'not-acceptable',
		    [], []}
	]}
]}
).

test_legacy_auth_request() ->
	testsuite:is(exmpp_client_legacy_auth:request("foobar", "dest"),
	    ?REQUEST1),
	ok.

test_legacy_auth_fields() ->
	testsuite:is(exmpp_server_legacy_auth:fields("foobar"),
	    ?FIELDS1),
	ok.

test_legacy_auth_password() ->
	testsuite:is(exmpp_client_legacy_auth:password("foobar",
	    "User", "Password", "Resource"),
	    ?PASSWORD1),
	testsuite:is(exmpp_client_legacy_auth:password_digest("foobar",
	    "User", "Password", "Resource"),
	    ?PASSWORD2),
	ok.

test_legacy_auth_success() ->
	testsuite:is(exmpp_server_legacy_auth:success("foobar"),
	    ?SUCCESS1),
	ok.

test_legacy_auth_failure() ->
	testsuite:is(exmpp_server_legacy_auth:failure("foobar",
	    not_authorized),
	    ?FAILURE1),
	testsuite:is(exmpp_server_legacy_auth:failure("foobar",
	    conflict),
	    ?FAILURE2),
	testsuite:is(exmpp_server_legacy_auth:failure("foobar",
	    not_acceptable),
	    ?FAILURE3),
	ok.
