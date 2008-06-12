% $Id$

-module(check_xml_parser).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    do_check(),
    testsuite:pass().

do_check() ->
    test_parser_start_stop(),
    test_parser_with_unknown_option(),
    test_parser_without_option(),
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
    test_parser_with_document_fragment(),
    test_parser_chunk_by_chunk(),
    test_parser_with_bad_xml(),
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
  {xmlnselement, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], [
    {xmlnselement, 'ns_default', [], "iq", [
      {xmlattr, ?NS_XML, "xml", "lang", "fr"}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_NS_ATOM, [
  {xmlnselement, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    'stream', [], [
    {xmlnselement, 'ns_default', [], 'iq', [
      {xmlattr, ?NS_XML, "xml", 'lang', "fr"}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_ROOT_DEPTH, [
  {xmlelement, "stream:stream", [
    {"xmlns:stream", "ns_stream"},
    {"xmlns", "ns_default"}
  ], undefined},
  {xmlelement, "iq", [
    {"xml:lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]}
]).

-define(TREE1_NS_ROOT_DEPTH, [
  {xmlnselement, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], undefined},
  {xmlnselement, 'ns_default', [], "iq", [
    {xmlattr, ?NS_XML, "xml", "lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]}
]).

-define(TREE1_END_EL, [
  {xmlelement, "stream:stream", [
    {"xmlns:stream", "ns_stream"},
    {"xmlns", "ns_default"}
  ], undefined},
  {xmlelement, "iq", [
    {"xml:lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]},
  {xmlendelement, "stream:stream"}
]).

-define(TREE1_NS_END_EL, [
  {xmlnselement, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], undefined},
  {xmlnselement, 'ns_default', [], "iq", [
    {xmlattr, ?NS_XML, "xml",
      "lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]},
  {xmlnsendelement, 'ns_stream', "stream", "stream"}
]).

-define(SOURCE2, "<element xmlns='unknown_ns' xmlns:stream='http://etherx.jabber.org/streams' xml:lang='fr' stream:version='1.0'/>").

-define(TREE2_NS_CHECK, [
  {xmlnselement, "unknown_ns",
    [{"unknown_ns", none}, {'http://etherx.jabber.org/streams',"stream"}],
    "element", [
    {xmlattr, ?NS_XML, "xml", "lang", "fr"},
    {xmlattr, ?NS_XMPP, "stream", "version", "1.0"}
  ], []}
]).

-define(SOURCE3, "<message><unknown/></message>").

-define(TREE3_NS_CHECK, [
  {xmlnselement, undefined, [], 'message', [], [
      {xmlnselement, undefined, [], "unknown", [], []}
  ]}
]).

-define(FRAGMENTS3, [
  {xmlelement, "message", [], undefined},
  {xmlelement, "unknown", [], undefined}
]).

-define(SOURCE4, "<stream version='1.0' foo='bar'/>").

-define(TREE4_NS_CHECK, [
  {xmlnselement, undefined, [], 'stream', [
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

-define(BAD_SOURCE1, "<stream attr=>").
-define(BAD_SOURCE2, "</stream>").

test_parser_start_stop() ->
    Parser = exmpp_xml:start_parser(),
    exmpp_xml:stop_parser(Parser),
    ok.

test_parser_with_unknown_option() ->
    try
        exmpp_xml:start_parser([bad_option]),
        testsuite:fail()
    catch
        throw:{xml_parser, options, invalid, bad_option} ->
            ok
    end,
    try
        exmpp_xml:parse_document("", [bad_option]),
        testsuite:fail()
    catch
        throw:{xml_parser, options, invalid, bad_option} ->
            ok
    end,
    ok.

test_parser_without_option() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1),
      ?TREE1_NO_NS),
    ok.

test_parser_with_no_namespace() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1, [no_namespace]),
      ?TREE1_NO_NS),
    ok.

test_parser_with_atom() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [name_as_atom, no_names_check, no_attrs_check]),
      ?TREE1_NO_NS_ATOM),
    ok.

test_parser_with_namespace() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [namespace, no_ns_check]),
      ?TREE1_NS),
    ok.

test_parser_with_namespace_and_atom() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [namespace, name_as_atom,
          no_ns_check, no_names_check, no_attrs_check]),
      ?TREE1_NS_ATOM),
    ok.

test_parser_with_ns_check() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE2,
        [namespace, ns_check]),
      ?TREE2_NS_CHECK),
    ok.

test_parser_with_names_check() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE3,
        [namespace, name_as_atom, names_check]),
      ?TREE3_NS_CHECK),
    ok.

test_parser_with_attrs_check() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE4,
        [namespace, name_as_atom, attrs_check]),
      ?TREE4_NS_CHECK),
    ok.

test_parser_with_data_inf_maxsize1() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [{maxsize, length(?SOURCE1)}]),
      ?TREE1_NO_NS),
    ok.

test_parser_with_data_inf_maxsize2() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [{maxsize, infinity}]),
      ?TREE1_NO_NS),
    ok.

test_parser_with_data_sup_maxsize() ->
    try
        exmpp_xml:parse_document(?SOURCE1, [{maxsize, length(?SOURCE1) - 1}]),
        testsuite:fail()
    catch
        throw:{xml_parser, parsing, stanza_too_big, undefined} ->
            ok
    end,
    ok.

test_parser_with_root_depth() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [{root_depth, 1}]),
      ?TREE1_ROOT_DEPTH),
    ok.

test_parser_with_ns_root_depth() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [namespace, {root_depth, 1}, no_ns_check]),
      ?TREE1_NS_ROOT_DEPTH),
    ok.

test_parser_with_end_element() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [{root_depth, 1}, endelement]),
      ?TREE1_END_EL),
    ok.

test_parser_with_ns_end_element() ->
    testsuite:is(exmpp_xml:parse_document(?SOURCE1,
        [namespace, {root_depth, 1}, endelement, no_ns_check]),
      ?TREE1_NS_END_EL),
    ok.

test_parser_with_document_fragment() ->
    testsuite:is(exmpp_xml:parse_document_fragment(?SOURCE3),
      ?FRAGMENTS3),
    try
        exmpp_xml:parse_document_fragment(?BAD_SOURCE1),
        testsuite:fail()
    catch
        throw:{xml_parser, parsing, malformed_xml, _} ->
            ok
    end,
    ok.

test_parser_chunk_by_chunk() ->
    Parser = exmpp_xml:start_parser(),
    testsuite:is(exmpp_xml:parse(Parser, ?CHUNK1),
      ?CHUNK1_TREE),
    testsuite:is(exmpp_xml:parse(Parser, ?CHUNK2),
      ?CHUNK2_TREE),
    testsuite:is(exmpp_xml:parse(Parser, ?CHUNK3),
      ?CHUNK3_TREE),
    exmpp_xml:stop_parser(Parser),
    ok.

test_parser_with_bad_xml() ->
    Parser = exmpp_xml:start_parser(),
    try
        exmpp_xml:parse(Parser, ?BAD_SOURCE1),
        testsuite:fail()
    catch
        throw:{xml_parser, parsing, malformed_xml, _} ->
            ok
    end,
    try
        exmpp_xml:parse_final(Parser, ?BAD_SOURCE2),
        testsuite:fail()
    catch
        throw:{xml_parser, parsing, malformed_xml, _} ->
            ok
    end,
    exmpp_xml:stop_parser(Parser),
    ok.
