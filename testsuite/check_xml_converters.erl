% $Id$

-module(check_xml_converters).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    do_check(),
    testsuite:pass().

do_check() ->
    test_encode_enities(),
    test_xmlnselement_to_xmlelement(),
    test_document_to_list_without_namespace(),
    test_document_to_list_with_namespace(),
    test_document_fragment_to_list_with_namespace(),
    test_document_fragment_to_list_with_namespace2(),
    test_clear_endelement_tuples(),
    ok.

% --------------------------------------------------------------------
% Serializer testsuite.
% --------------------------------------------------------------------

-define(TREE0_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"}
    ], undefined}
).

-define(TREE0_NS,
  {xmlnselement, 'ns_stream', "stream", undefined, "stream", [], undefined}
).

-define(TREE0_NS_NAA,
  {xmlnselement, 'ns_stream', "stream", undefined, 'stream', [], undefined}
).

-define(SOURCE0, "<stream:stream xmlns:stream=\"ns_stream\">").

-define(TREE1_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"}
    ], []}
).

-define(TREE1_NS,
  {xmlnselement, 'ns_stream', "stream", undefined, "stream", [], []}
).

-define(SOURCE1, "<stream:stream xmlns:stream=\"ns_stream\"/>").

-define(TREE2_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"},
      {"xmlns", "ns_default"}
    ], [
      {xmlelement, "iq", [
          {"xml:lang", "fr"},
          {"version", "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(TREE2_NS,
  {xmlnselement, 'ns_stream', "stream", 'ns_default', "stream", [], [
      {xmlnselement, 'ns_default', undefined, undefined, "iq", [
          {xmlattr, ?NS_XML, undefined, "lang", "fr"},
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(TREE2_NS_NAA,
  {xmlnselement, 'ns_stream', "stream", 'ns_default', 'stream', [], [
      {xmlnselement, 'ns_default', undefined, undefined, 'iq', [
          {xmlattr, ?NS_XML, undefined, 'lang', "fr"},
          {xmlattr, undefined, undefined, 'version', "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(SOURCE2, "<stream:stream xmlns:stream=\"ns_stream\" xmlns=\"ns_default\"><iq xml:lang=\"fr\" version=\"1.0\">Content</iq></stream:stream>").

-define(TREE3_NS,
  {xmlnselement, 'ns_iq', undefined, undefined, "iq", [
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}
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
  {xmlnselement, undefined, undefined, undefined, "stream", [
      {xmlattr, 'ns_attr', "pfx", "foo", "bar"}
    ], []}
).

-define(TREE5_NO_NS,
  {xmlendelement, "stream:stream"}
).

-define(TREE5_NO_NS2,
  {xmlendelement, "stream"}
).

-define(TREE5_NO_NS3,
  {xmlendelement, "stream2:stream"}
).

-define(TREE5_NS,
  {xmlnsendelement, 'ns_stream', "stream", "stream"}
).

-define(TREE5_NS2,
  {xmlnsendelement, 'ns_stream', undefined, "stream"}
).

-define(TREE5_NS3,
  {xmlnsendelement, undefined, undefined, "stream"}
).

-define(TREE5_NS_NAA,
  {xmlnsendelement, 'ns_stream', "stream", 'stream'}
).

-define(SOURCE5, "</stream:stream>").

test_xmlnselement_to_xmlelement() ->
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE0_NO_NS),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE0_NS),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE0_NS_NAA),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE1_NO_NS),
      ?TREE1_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE1_NS),
      ?TREE1_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NO_NS),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NS),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE2_NS_NAA),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE4_NO_NS),
      ?TREE4_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE4_NS),
      ?TREE4_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NO_NS),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS_NAA),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS,
        ['ns_stream', 'other'], []),
      ?TREE5_NO_NS2),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS,
        [], [{'ns_stream', "stream2"}]),
      ?TREE5_NO_NS3),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS2),
      ?TREE5_NO_NS2),
    testsuite:is(exmpp_xml:xmlnselement_to_xmlelement(?TREE5_NS3),
      ?TREE5_NO_NS2),
    ok.

test_document_to_list_without_namespace() ->
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE0_NO_NS)),
      ?SOURCE0),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE1_NO_NS)),
      ?SOURCE1),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE2_NO_NS)),
      ?SOURCE2),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NO_NS)),
      ?SOURCE5),
    ok.

test_document_to_list_with_namespace() ->
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE0_NS)),
      ?SOURCE0),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE1_NS)),
      ?SOURCE1),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE2_NS)),
      ?SOURCE2),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NS)),
      ?SOURCE5),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NS_NAA)),
      ?SOURCE5),
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

test_encode_enities() ->
    testsuite:is(exmpp_xml:encode_entities("Entities: &<>\"'"),
      "Entities: &amp;&lt;&gt;&quot;&apos;"),
    testsuite:is(exmpp_xml:encode_entities(<<"Entities: &<>\"'">>),
      <<"Entities: &amp;&lt;&gt;&quot;&apos;">>),
    ok.

test_clear_endelement_tuples() ->
    testsuite:is(exmpp_xml:clear_endelement_tuples([?TREE5_NO_NS]), []),
    testsuite:is(exmpp_xml:clear_endelement_tuples(
        [?TREE0_NO_NS, ?TREE5_NO_NS]), [?TREE0_NO_NS]),
    testsuite:is(exmpp_xml:clear_endelement_tuples([?TREE5_NS]), []),
    testsuite:is(exmpp_xml:clear_endelement_tuples(
        [?TREE0_NS, ?TREE5_NS]), [?TREE0_NS]),
    ok.
