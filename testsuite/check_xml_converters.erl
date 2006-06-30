% $Id$

-module(check_xml_converters).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_xmlnselement_to_xmlelement(),
	test_document_to_list_without_namespace(),
	test_document_to_list_with_namespace(),
	test_document_fragment_to_list_with_namespace(),
	test_document_fragment_to_list_with_namespace2(),
	ok.

% --------------------------------------------------------------------
% Serliazer testsuite.
% --------------------------------------------------------------------

-define(XML_NS, 'http://www.w3.org/XML/1998/namespace').

-define(TREE1_NO_NS,
{xmlelement, "stream:stream", [
	{"xmlns:stream", "ns_stream"}
], []}
).

-define(TREE1_NS,
{xmlnselement, 'ns_stream', "stream", "stream", [], []}
).

-define(SOURCE1, "<stream:stream xmlns:stream=\"ns_stream\"/>").

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
		{xmlattr, ?XML_NS, undefined, "lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
).

-define(TREE2_NS_NAS,
{xmlnselement, 'ns_stream', "stream", 'stream', [], [
	{xmlnselement, 'ns_default', undefined, 'iq', [
		{xmlattr, ?XML_NS, undefined, 'lang', "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
).

-define(SOURCE2, "<stream:stream xmlns:stream=\"ns_stream\"><iq xmlns=\"ns_default\" xml:lang=\"fr\">Content</iq></stream:stream>").

-define(TREE3_NS,
{xmlnselement, 'ns_iq', undefined, "iq", [
	{xmlattr, ?XML_NS, undefined, "lang", "fr"}
], [
	{xmlcdata, <<"Binary">>},
	{xmlcdata, "List"},
	{xmlcdata, <<"& < > \" '">>}
]}
).

-define(TREE3_DEFAULT_NS, [
	'ns_iq'
]).

-define(TREE3_PREFIXED_NS, [
]).

-define(SOURCE3, "<iq xml:lang=\"fr\">BinaryList&amp; &lt; &gt; &quot; &apos;</iq>").

-define(TREE3_DEFAULT_NS_2, [
]).

-define(TREE3_PREFIXED_NS_2, [
	{'ns_iq', "jabber"}
]).

-define(SOURCE3_2, "<jabber:iq xml:lang=\"fr\">BinaryList&amp; &lt; &gt; &quot; &apos;</jabber:iq>").

-define(TREE4_NO_NS,
{xmlelement, "stream", [
	{"xmlns:pfx", "ns_attr"},
	{"pfx:foo", "bar"}
], []}
).

-define(TREE4_NS,
{xmlnselement, undefined, undefined, "stream", [
	{xmlattr, 'ns_attr', "pfx", "foo", "bar"}
], []}
).

test_xmlnselement_to_xmlelement() ->
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE1_NO_NS),
	    ?TREE1_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE1_NS),
	    ?TREE1_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NO_NS),
	    ?TREE2_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NS),
	    ?TREE2_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NS_NAS),
	    ?TREE2_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE4_NO_NS),
	    ?TREE4_NO_NS),
	testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE4_NS),
	    ?TREE4_NO_NS),
	ok.

test_document_to_list_without_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE1_NO_NS)),
	    ?SOURCE1),
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE2_NO_NS)),
	    ?SOURCE2),
	ok.

test_document_to_list_with_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE1_NS)),
	    ?SOURCE1),
	testsuite:is(
	    lists:flatten(exmpp_xml:document_to_list(?TREE2_NS)),
	    ?SOURCE2),
	ok.

test_document_fragment_to_list_with_namespace() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_DEFAULT_NS, ?TREE3_PREFIXED_NS)),
	    ?SOURCE3),
	ok.

test_document_fragment_to_list_with_namespace2() ->
	testsuite:is(
	    lists:flatten(exmpp_xml:document_fragment_to_list(?TREE3_NS,
	    ?TREE3_DEFAULT_NS_2, ?TREE3_PREFIXED_NS_2)),
	    ?SOURCE3_2),
	ok.
