% $Id$

-module(check_xmlstream).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    do_check(),
    testsuite:pass().

do_check() ->
    test_parse_stream(),
    test_parse_stream_ns(),
    test_parse_stream_no_callback(),
    test_parse_stream_no_callback_ns(),
    ok.

% --------------------------------------------------------------------
% Stream testsuite.
% --------------------------------------------------------------------

-define(CHUNK1, "<stream xml:lang='fr' version='1.0'><mess").
-define(CHUNK2, "age>Content</mess").
-define(CHUNK3, "age></stream>").

-define(CHUNK_EVENT1,
  {xmlstreamstart,
    {xmlelement, "stream", [
        {"xml:lang", "fr"},
        {"version", "1.0"}
      ], undefined}
  }).

-define(CHUNK_EVENT2,
  {xmlstreamelement,
    {xmlelement, "message", [], [
        {xmlcdata, <<"Content">>}
      ]}
  }).

-define(CHUNK_EVENT3,
  {xmlstreamend,
    {xmlendtag, undefined, undefined, "stream"}
  }).

-define(CHUNK_NS_EVENT1,
  {xmlstreamstart,
    {xmlel, undefined, [], 'stream', [
        {xmlattr, ?NS_XML, "xml", 'lang', "fr"},
        {xmlattr, undefined, undefined, 'version', "1.0"}
      ], undefined}
  }).

-define(CHUNK_NS_EVENT2,
  {xmlstreamelement,
    {xmlel, undefined, [], 'message', [], [
        {xmlcdata, <<"Content">>}
      ]}
  }).

-define(CHUNK_NS_EVENT3,
  {xmlstreamend,
    {xmlendtag, undefined, undefined, 'stream'}
  }).

test_parse_stream() ->
    S1 = exmpp_xmlstream:start({apply, {?MODULE, test_parse_stream_p1, []}},
      [], [{xmlstreamstart, new}]),
    {ok, S2} = exmpp_xmlstream:parse(S1, ?CHUNK1),
    {ok, S3} = exmpp_xmlstream:parse(S2, ?CHUNK2),
    {ok, S4} = exmpp_xmlstream:parse(S3, ?CHUNK3),
    exmpp_xmlstream:stop(S4),
    ok.

test_parse_stream_ns() ->
    S1 = exmpp_xmlstream:start({apply, {?MODULE, test_parse_stream_p2, []}},
      [namespace, name_as_atom], [{xmlstreamstart, new}]),
    {ok, S2} = exmpp_xmlstream:parse(S1, ?CHUNK1),
    {ok, S3} = exmpp_xmlstream:parse(S2, ?CHUNK2),
    {ok, S4} = exmpp_xmlstream:parse(S3, ?CHUNK3),
    exmpp_xmlstream:stop(S4),
    ok.

test_parse_stream_p1(Ret, _Extra) ->
    case Ret of
        ?CHUNK_EVENT1 ->
            ok;
        ?CHUNK_EVENT2 ->
            ok;
        ?CHUNK_EVENT3 ->
            ok;
        _ ->
            testsuite:fail({no_match, Ret})
    end.

test_parse_stream_p2(Ret, _Extra) ->
    case Ret of
        ?CHUNK_NS_EVENT1 ->
            ok;
        ?CHUNK_NS_EVENT2 ->
            ok;
        ?CHUNK_NS_EVENT3 ->
            ok;
        _ ->
            testsuite:fail({no_match, Ret})
    end.

test_parse_stream_no_callback() ->
    S1 = exmpp_xmlstream:start(no_callback, [], [{xmlstreamstart, new}]),
    {ok, S2, Ret1} = exmpp_xmlstream:parse(S1, ?CHUNK1),
    testsuite:is(Ret1, [?CHUNK_EVENT1]),
    {ok, S3, Ret2} = exmpp_xmlstream:parse(S2, ?CHUNK2),
    testsuite:is(Ret2, []),
    {ok, S4, Ret3} = exmpp_xmlstream:parse(S3, ?CHUNK3),
    testsuite:is(Ret3, [?CHUNK_EVENT2, ?CHUNK_EVENT3]),
    exmpp_xmlstream:stop(S4),
    ok.

test_parse_stream_no_callback_ns() ->
    S1 = exmpp_xmlstream:start(no_callback, [namespace, name_as_atom],
      [{xmlstreamstart, new}]),
    {ok, S2, Ret1} = exmpp_xmlstream:parse(S1, ?CHUNK1),
    testsuite:is(Ret1, [?CHUNK_NS_EVENT1]),
    {ok, S3, Ret2} = exmpp_xmlstream:parse(S2, ?CHUNK2),
    testsuite:is(Ret2, []),
    {ok, S4, Ret3} = exmpp_xmlstream:parse(S3, ?CHUNK3),
    testsuite:is(Ret3, [?CHUNK_NS_EVENT2, ?CHUNK_NS_EVENT3]),
    exmpp_xmlstream:stop(S4),
    ok.
