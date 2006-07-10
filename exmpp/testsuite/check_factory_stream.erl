% $Id$

-module(check_factory_stream).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_stream_opening(),
	test_stream_opening_reply(),
	test_stream_closing(),
	ok.

% --------------------------------------------------------------------
% Serliazer testsuite.
% --------------------------------------------------------------------

-define(OPENING1,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"}
], undefined}
).

-define(OPENING2,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:server"}
], undefined}
).

-define(OPENING3,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'to', "dest"}
], undefined}
).

-define(OPENING4,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, ?NS_XML, undefined, 'lang', "fr"}
], undefined}
).

-define(OPENING5,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'version', "1.0"}
], undefined}
).

-define(OPENING_REPLY1,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], undefined}
).

-define(OPENING_REPLY2,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:server"},
	{xmlattr, undefined, undefined, 'id', "foobar"}
], undefined}
).

-define(OPENING_REPLY3,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'id', "foobar"},
	{xmlattr, undefined, undefined, 'from', "orig"}
], undefined}
).

-define(OPENING_REPLY4,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'id', "foobar"},
	{xmlattr, ?NS_XML, undefined, 'lang', "fr"}
], undefined}
).

-define(OPENING_REPLY5,
{xmlnselement, ?NS_XMPP, "stream", 'stream', [
	{xmlattr, undefined, undefined, 'xmlns', "jabber:client"},
	{xmlattr, undefined, undefined, 'id', "foobar"},
	{xmlattr, undefined, undefined, 'version', "1.0"}
], undefined}
).

-define(CLOSING1,
{xmlnsendelement, ?NS_XMPP, "stream", 'stream'}
).

test_stream_opening() ->
	testsuite:is(exmpp_factory:stream_opening([]),
	    {error, unspecified_context}),
	testsuite:is(exmpp_factory:stream_opening([bad_arg]),
	    {error, {unknown_argument, bad_arg}}),
	testsuite:is(exmpp_factory:stream_opening([{context, bad}]),
	    {error, {unknown_context, bad}}),
	testsuite:is(exmpp_factory:stream_opening([
	    {context, client}
	    ]), ?OPENING1),
	testsuite:is(exmpp_factory:stream_opening([
	    {context, server}
	    ]), ?OPENING2),
	testsuite:is(exmpp_factory:stream_opening([
	    {context, client},
	    {to, "dest"}
	    ]), ?OPENING3),
	testsuite:is(exmpp_factory:stream_opening([
	    {context, client},
	    {lang, "fr"}
	    ]), ?OPENING4),
	testsuite:is(exmpp_factory:stream_opening([
	    {context, client},
	    {version, "1.0"}
	    ]), ?OPENING5),
	ok.

test_stream_opening_reply() ->
	testsuite:is(exmpp_factory:stream_opening_reply([]),
	    {error, unspecified_context}),
	testsuite:is(exmpp_factory:stream_opening_reply([bad_arg]),
	    {error, {unknown_argument, bad_arg}}),
	testsuite:is(exmpp_factory:stream_opening_reply([{context, bad}]),
	    {error, {unknown_context, bad}}),
	testsuite:is(exmpp_factory:stream_opening_reply([
	    {context, client},
	    {id, "foobar"}
	    ]), ?OPENING_REPLY1),
	testsuite:is(exmpp_factory:stream_opening_reply([
	    {context, server},
	    {id, "foobar"}
	    ]), ?OPENING_REPLY2),
	testsuite:is(exmpp_factory:stream_opening_reply([
	    {context, client},
	    {id, "foobar"},
	    {from, "orig"}
	    ]), ?OPENING_REPLY3),
	testsuite:is(exmpp_factory:stream_opening_reply([
	    {context, client},
	    {id, "foobar"},
	    {lang, "fr"}
	    ]), ?OPENING_REPLY4),
	testsuite:is(exmpp_factory:stream_opening_reply([
	    {context, client},
	    {id, "foobar"},
	    {version, "1.0"}
	    ]), ?OPENING_REPLY5),
	ok.

test_stream_closing() ->
	testsuite:is(exmpp_factory:stream_closing(),
	    ?CLOSING1),
	testsuite:is(exmpp_factory:stream_closing(?OPENING1),
	    ?CLOSING1),
	ok.
