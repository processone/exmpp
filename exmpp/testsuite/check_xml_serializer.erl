% $Id$

-module(check_xml_serializer).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0]).

check() ->
	testsuite:ok(test_document_to_list_without_namespace()),
	testsuite:ok(test_document_to_list_with_namespace()),
	testsuite:ok(test_document_fragment_to_list_with_namespace()),
	testsuite:ok(test_document_fragment_to_list_with_namespace2()),
	testsuite:pass().

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
{xmlnselement, 'ns_default', undefined, "iq", [
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
	    "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]}
).

-define(TREE3_NS_STACK, [
	'ns_default',
	'ns_stream'
]).

-define(TREE3_PREFIX_STACK, [
	undefined,
	"stream"
]).

-define(SOURCE3, "<iq xml:lang=\"fr\">Content</iq>").

-define(TREE3_NS_STACK_2, [
	'ns_default',
	'ns_stream'
]).

-define(TREE3_PREFIX_STACK_2, [
	"jabber",
	"stream"
]).

-define(SOURCE3_2, "<jabber:iq xml:lang=\"fr\">Content</jabber:iq>").

test_document_to_list_without_namespace() ->
	case lists:flatten(exmpp_xml:document_to_list(?TREE2_NO_NS)) of
		?SOURCE2 ->
			ok;
		List ->
			{test_failed, List, ?SOURCE2}
	end.

test_document_to_list_with_namespace() ->
	case lists:flatten(exmpp_xml:document_to_list(?TREE2_NS)) of
		?SOURCE2 ->
			ok;
		List ->
			{test_failed, List, ?SOURCE2}
	end.

test_document_fragment_to_list_with_namespace() ->
	case lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_NS_STACK, ?TREE3_PREFIX_STACK)) of
		?SOURCE3 ->
			ok;
		List ->
			{test_failed, List, ?SOURCE3}
	end.

test_document_fragment_to_list_with_namespace2() ->
	case lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_NS_STACK_2, ?TREE3_PREFIX_STACK_2)) of
		?SOURCE3_2 ->
			ok;
		List ->
			{test_failed, List, ?SOURCE3_2}
	end.
