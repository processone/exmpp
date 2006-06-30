% $Id$

-module(check_xml_serializer).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_document_to_list_without_namespace(),
	test_document_to_list_with_namespace(),
	test_document_fragment_to_list_with_namespace(),
	test_document_fragment_to_list_with_namespace2(),
	ok.

% --------------------------------------------------------------------
% Serliazer testsuite.
% --------------------------------------------------------------------

-define(TREE2_NO_NS,
{xmlelement, "stream:stream", [
	{"xmlns:stream", "ns_stream"}
], [
	{xmlelement, "iq", [
		{"xmlns", "ns_default"},
		{"xml:lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
).

-define(TREE2_NS,
{xmlnselement, 'ns_stream', "stream", "stream", [], [
	{xmlnselement, 'ns_default', undefined, "iq", [
		{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
		    "lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
).

-define(SOURCE2, "<stream:stream xmlns:stream=\"ns_stream\"><iq xmlns=\"ns_default\" xml:lang=\"fr\">Content</iq></stream:stream>").

-define(TREE3_NS,
{xmlnselement, 'ns_iq', undefined, "iq", [
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
	    "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]}
).

-define(TREE3_DEFAULT_NS, [
	'ns_iq'
]).

-define(TREE3_PREFIXED_NS, [
]).

-define(SOURCE3, "<iq xml:lang=\"fr\">Content</iq>").

-define(TREE3_DEFAULT_NS_2, [
]).

-define(TREE3_PREFIXED_NS_2, [
	{'ns_iq', "jabber"}
]).

-define(SOURCE3_2, "<jabber:iq xml:lang=\"fr\">Content</jabber:iq>").

test_document_to_list_without_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE2_NO_NS)),
	    ?SOURCE2).

test_document_to_list_with_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE2_NS)),
	    ?SOURCE2).

test_document_fragment_to_list_with_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_DEFAULT_NS, ?TREE3_PREFIXED_NS)),
	    ?SOURCE3).

test_document_fragment_to_list_with_namespace2() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_DEFAULT_NS_2, ?TREE3_PREFIXED_NS_2)),
	    ?SOURCE3_2).
