% $Id$

-module(xml_parser).
-vsn('$Revision$').

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

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
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], [
    {xmlel, 'ns_default', [], "iq", [
      {xmlattr, ?NS_XML, "xml", "lang", "fr"}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_NS_ATOM, [
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    'stream', [], [
    {xmlel, 'ns_default', [], 'iq', [
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
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], undefined},
  {xmlel, 'ns_default', [], "iq", [
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
  {xmlendtag, undefined, undefined, "stream:stream"}
]).

-define(TREE1_NS_END_EL, [
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], undefined},
  {xmlel, 'ns_default', [], "iq", [
    {xmlattr, ?NS_XML, "xml",
      "lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]},
  {xmlendtag, 'ns_stream', "stream", "stream"}
]).

-define(SOURCE2, "<element xmlns='unknown_ns' xmlns:stream='http://etherx.jabber.org/streams' xml:lang='fr' stream:version='1.0'/>").

-define(TREE2_NS_CHECK, [
  {xmlel, "unknown_ns",
    [{"unknown_ns", none}, {'http://etherx.jabber.org/streams',"stream"}],
    "element", [
    {xmlattr, ?NS_XML, "xml", "lang", "fr"},
    {xmlattr, ?NS_XMPP, "stream", "version", "1.0"}
  ], []}
]).

-define(SOURCE3, "<message><unknown/></message>").

-define(TREE3_NS_CHECK, [
  {xmlel, undefined, [], 'message', [], [
      {xmlel, undefined, [], "unknown", [], []}
  ]}
]).

-define(FRAGMENTS3, [
  {xmlelement, "message", [], undefined},
  {xmlelement, "unknown", [], undefined}
]).

-define(SOURCE4, "<stream version='1.0' foo='bar'/>").

-define(TREE4_NS_CHECK, [
  {xmlel, undefined, [], 'stream', [
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

start_stop_test_() ->
    ?_assertMatch(ok, exmpp_xml:stop_parser(exmpp_xml:start_parser())).

unknown_options_test_() ->
    [
      ?_assertThrow(
        {xml_parser, options, invalid, _Infos},
        exmpp_xml:start_parser([bad_option])
      ),
      ?_assertThrow(
        {xml_parser, options, invalid, _Infos},
        exmpp_xml:parse_document("", [bad_option])
      )
    ].

options_test_() ->
    [
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1)
      ),
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{namespace, false}])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS_ATOM,
        exmpp_xml:parse_document(?SOURCE1,
          [name_as_atom, {names_check, false}, {attrs_check, false}])
      ),
      ?_assertMatch(
        ?TREE1_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [namespace, {ns_check, false}])
      ),
      ?_assertMatch(
        ?TREE1_NS_ATOM,
        exmpp_xml:parse_document(?SOURCE1,
          [namespace, name_as_atom,
            {ns_check, false}, {names_check, false}, {attrs_check, false}])
      ),
      ?_assertMatch(
        ?TREE2_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE2,
          [namespace, ns_check])
      ),
      ?_assertMatch(
        ?TREE3_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE3,
          [namespace, name_as_atom, names_check])
      ),
      ?_assertMatch(
        ?TREE4_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE4,
          [namespace, name_as_atom, attrs_check])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{maxsize, length(?SOURCE1)}])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{maxsize, infinity}])
      ),
      ?_assertThrow(
        {xml_parser, parsing, stanza_too_big, undefined},
        exmpp_xml:parse_document(?SOURCE1,
          [{maxsize, length(?SOURCE1) - 1}])
      ),
      ?_assertMatch(
        ?TREE1_ROOT_DEPTH,
        exmpp_xml:parse_document(?SOURCE1,
          [{root_depth, 1}])
      ),
      ?_assertMatch(
        ?TREE1_NS_ROOT_DEPTH,
        exmpp_xml:parse_document(?SOURCE1,
          [namespace, {root_depth, 1}, {ns_check, false}])
      ),
      ?_assertMatch(
        ?TREE1_END_EL,
        exmpp_xml:parse_document(?SOURCE1,
          [{root_depth, 1}, endtag])
      ),
      ?_assertMatch(
        ?TREE1_NS_END_EL,
        exmpp_xml:parse_document(?SOURCE1,
          [namespace, {root_depth, 1}, endtag, {ns_check, false}])
      )
    ].

chunk_by_chunk_test_() ->
    Setup = fun() -> exmpp_xml:start_parser() end,
    Cleanup = fun(P) -> exmpp_xml:stop_parser(P) end,
    Inst = {with, [
        fun(P) ->
            ?assertMatch(?CHUNK1_TREE, exmpp_xml:parse(P, ?CHUNK1))
        end,
        fun(P) ->
            ?assertMatch(?CHUNK2_TREE, exmpp_xml:parse(P, ?CHUNK2))
        end,
        fun(P) ->
            ?assertMatch(?CHUNK3_TREE, exmpp_xml:parse(P, ?CHUNK3))
        end
    ]},
    {setup, Setup, Cleanup, Inst}.

bad_xml_test_() ->
    Setup = fun() -> exmpp_xml:start_parser() end,
    Cleanup = fun(P) -> exmpp_xml:stop_parser(P) end,
    Inst = {with, [
        fun(P) ->
            ?assertThrow(
              {xml_parser, parsing, malformed_xml, _},
              exmpp_xml:parse(P, ?BAD_SOURCE1)
            )
        end,
        fun(P) ->
            ?assertThrow(
              {xml_parser, parsing, malformed_xml, _},
              exmpp_xml:parse_final(P, ?BAD_SOURCE2)
            )
        end
    ]},
    {setup, Setup, Cleanup, Inst}.
