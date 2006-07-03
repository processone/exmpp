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
	test_parser_with_no_namespace(),
	test_parser_with_atom(),
	test_parser_with_namespace(),
	test_parser_with_namespace_and_atom(),
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
		{xmlattr, 'http://www.w3.org/XML/1998/namespace', "xml",
		    "lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_NS_ATOM, [
{xmlnselement, 'ns_stream', "stream", 'stream', [], [
	{xmlnselement, 'ns_default', undefined, 'iq', [
		{xmlattr, 'http://www.w3.org/XML/1998/namespace', "xml",
		    'lang', "fr"}
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
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', "xml",
	    "lang", "fr"}
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
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', "xml",
	    "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]},
{xmlnsendelement, 'ns_stream', "stream"}
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

test_parser_with_no_namespace() ->
	{ok, Parser} = exmpp_xml:start_parser([no_namespace]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_atom() ->
	{ok, Parser} = exmpp_xml:start_parser([name_as_atom]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NO_NS_ATOM}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_namespace() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS}),
	exmpp_xml:stop_parser(Parser),
	ok.

test_parser_with_namespace_and_atom() ->
	{ok, Parser} = exmpp_xml:start_parser([namespace, name_as_atom]),
	testsuite:is(exmpp_xml:parse_final(Parser, ?SOURCE1),
	    {ok, ?TREE1_NS_ATOM}),
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
	{ok, Parser} = exmpp_xml:start_parser([namespace, {root_depth, 1}]),
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
	    endelement]),
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
