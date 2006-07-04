% $Id$

-module(check_xml_parser).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_parser_without_option(),
	test_parser_with_unknown_option(),
	test_parser_with_no_namespace(),
	test_parser_with_atom(),
	test_parser_with_namespace(),
	test_parser_with_namespace_and_atom(),
	test_parser_with_ns_check(),
	test_parser_with_names_check(),
	test_parser_with_attrs_check(),
	test_parser_with_data_inf_maxsize1(),
	test_parser_with_data_inf_maxsize2(),
	test_parser_with_data_sup_maxsize(),
	test_parser_with_root_depth(),
	test_parser_with_ns_root_depth(),
	test_parser_with_end_element(),
	test_parser_with_ns_end_element(),
	test_parser_chunk_by_chunk(),
	ok.

% --------------------------------------------------------------------
% Parser testsuite.
% --------------------------------------------------------------------

-define(XML_NS, 'http://www.w3.org/XML/1998/namespace').
-define(XMPP_NS, 'http://etherx.jabber.org/streams').

-define(SOURCE1, "<stream:stream xmlns:stream='ns_stream' xmlns='ns_default'><iq xml:lang='fr'>Content</iq></stream:stream>").

-define(TREE1_NO_NS, [
{xmlelement, "stream:stream", [
	{"xmlns:stream", "ns_stream"},
	{"xmlns", "ns_default"}
], [
	{xmlelement, "iq", [
		{"xml:lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_NO_NS_ATOM, [
{xmlelement, 'stream:stream', [
	{'xmlns:stream', "ns_stream"},
	{'xmlns', "ns_default"}
], [
	{xmlelement, 'iq', [
		{'xml:lang', "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_NS, [
{xmlnselement, 'ns_stream', "stream", "stream", [], [
	{xmlnselement, 'ns_default', undefined, "iq", [
		{xmlattr, ?XML_NS, "xml", "lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_NS_ATOM, [
{xmlnselement, 'ns_stream', "stream", 'stream', [], [
	{xmlnselement, 'ns_default', undefined, 'iq', [
		{xmlattr, ?XML_NS, "xml", 'lang', "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_ROOT_DEPTH, [
{xmlelement, "stream:stream", [
	{"xmlns:stream", "ns_stream"},
	{"xmlns", "ns_default"}
], []},
{xmlelement, "iq", [
	{"xml:lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]}
]).

-define(TREE1_NS_ROOT_DEPTH, [
{xmlnselement, 'ns_stream', "stream", "stream", [], []},
{xmlnselement, 'ns_default', undefined, "iq", [
	{xmlattr, ?XML_NS, "xml", "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]}
]).

-define(TREE1_END_EL, [
{xmlelement, "stream:stream", [
	{"xmlns:stream", "ns_stream"},
	{"xmlns", "ns_default"}
], []},
{xmlelement, "iq", [
	{"xml:lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]},
{xmlendelement, "stream:stream"}
]).

-define(TREE1_NS_END_EL, [
{xmlnselement, 'ns_stream', "stream", "stream", [], []},
{xmlnselement, 'ns_default', undefined, "iq", [
	{xmlattr, ?XML_NS, "xml",
	    "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]},
{xmlnsendelement, 'ns_stream', "stream"}
]).

-define(SOURCE2, "<element xmlns='unknown_ns' xmlns:stream='http://etherx.jabber.org/streams' xml:lang='fr' stream:version='1.0'/>").

-define(TREE2_NS_CHECK, [
{xmlnselement, "unknown_ns", undefined, "element", [
	{xmlattr, ?XML_NS, "xml", "lang", "fr"},
	{xmlattr, ?XMPP_NS, "stream", "version", "1.0"}
], []}
]).

-define(SOURCE3, "<message><unknown/></message>").

-define(TREE3_NS_CHECK, [
{xmlnselement, undefined, undefined, 'message', [], [
	{xmlnselement, undefined, undefined, "unknown", [], []}
]}
]).

-define(SOURCE4, "<stream version='1.0' foo='bar'/>").

-define(TREE4_NS_CHECK, [
{xmlnselement, undefined, undefined, 'stream', [
	{xmlattr, undefined, undefined, 'version', "1.0"},
	{xmlattr, undefined, undefined, "foo", "bar"}
], []}
]).

-define(CHUNK1, "").
-define(CHUNK2, "<stream xml:lang='fr' version='1.0'>Content</strea").
-define(CHUNK3, "m>").

-define(CHUNK1_TREE, continue).
-define(CHUNK2_TREE, continue).
-define(CHUNK3_TREE, [
{xmlelement, "stream", [
	{"xml:lang", "fr"},
	{"version", "1.0"}
], [
	{xmlcdata, <<"Content">>}
]}
]).

test_parser_without_option() ->
	{ok, Parser} = exmpp_xml:start_parser(),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_unknown_option() ->
	testsuite:is(exmpp_xml:start_parser([bad_option]),
	    {error, {unknown_option, bad_option}}),
	ok.

test_parser_with_no_namespace() ->
	{ok, Parser} = exmpp_xml:start_parser([no_namespace]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_atom() ->
	{ok, Parser} = exmpp_xml:start_parser([name_as_atom,
	    no_names_check, no_attrs_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS_ATOM}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_namespace() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace,
	    no_ns_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_namespace_and_atom() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, name_as_atom,
	    no_ns_check, no_names_check, no_attrs_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS_ATOM}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_ns_check() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, ns_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE2),
	    {ok, ?TREE2_NS_CHECK}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_names_check() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, name_as_atom,
	    names_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE3),
	    {ok, ?TREE3_NS_CHECK}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_attrs_check() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, name_as_atom,
	    attrs_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE4),
	    {ok, ?TREE4_NS_CHECK}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_data_inf_maxsize1() ->
	{ok, Parser} = exmpp_xml:start_parser([{maxsize, length(?SOURCE1)}]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_data_inf_maxsize2() ->
	{ok, Parser} = exmpp_xml:start_parser([{maxsize, infinity}]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_data_sup_maxsize() ->
	{ok, Parser} = exmpp_xml:start_parser([{maxsize,
	    length(?SOURCE1) - 1}]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {error, stanza_too_big}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_root_depth() ->
	{ok, Parser} = exmpp_xml:start_parser([{root_depth, 1}]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_ROOT_DEPTH}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_ns_root_depth() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, {root_depth, 1},
	    no_ns_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS_ROOT_DEPTH}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_end_element() ->
	{ok, Parser} = exmpp_xml:start_parser([{root_depth, 1}, endelement]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_END_EL}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_ns_end_element() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, {root_depth, 1},
	    endelement, no_ns_check]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS_END_EL}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_chunk_by_chunk() ->
	{ok, Parser} = exmpp_xml:start_parser(),
	testsuite:is(exmpp_xml:parse(Parser, ?CHUNK1),
	    {ok, ?CHUNK1_TREE}),
	testsuite:is(exmpp_xml:parse(Parser, ?CHUNK2),
	    {ok, ?CHUNK2_TREE}),
	testsuite:is(exmpp_xml:parse(Parser, ?CHUNK3),
	    {ok, ?CHUNK3_TREE}),
	exmpp_xml:stop_parser(Parser),
	ok.
