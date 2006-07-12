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
	{xmlendelement, "stream"}
}).

-define(CHUNK_NS_EVENT1,
{xmlstreamstart,
	{xmlnselement, undefined, undefined, 'stream', [
		{xmlattr, ?NS_XML, "xml", 'lang', "fr"},
		{xmlattr, undefined, undefined, 'version', "1.0"}
	], undefined}
}).

-define(CHUNK_NS_EVENT2,
{xmlstreamelement,
	{xmlnselement, undefined, undefined, 'message', [], [
		{xmlcdata, <<"Content">>}
	]}
}).

-define(CHUNK_NS_EVENT3,
{xmlstreamend,
	{xmlnsendelement, undefined, undefined, 'stream'}
}).

test_parse_stream() ->
	case exmpp_xmlstream:start(
	    {apply, {?MODULE, test_parse_stream_p1, []}}) of
		{ok, S1} ->
			{ok, S2} = exmpp_xmlstream:parse(S1, ?CHUNK1),
			{ok, S3} = exmpp_xmlstream:parse(S2, ?CHUNK2),
			{ok, S4} = exmpp_xmlstream:parse(S3, ?CHUNK3),
			exmpp_xmlstream:stop(S4);
		Other1 ->
			testsuite:fail({start_error, Other1})
	end,
	case exmpp_xmlstream:start(
	    {apply, {?MODULE, test_parse_stream_p2, []}},
	    [namespace, name_as_atom]) of
		{ok, S5} ->
			{ok, S6} = exmpp_xmlstream:parse(S5, ?CHUNK1),
			{ok, S7} = exmpp_xmlstream:parse(S6, ?CHUNK2),
			{ok, S8} = exmpp_xmlstream:parse(S7, ?CHUNK3),
			exmpp_xmlstream:stop(S8);
		Other2 ->
			testsuite:fail({start_error, Other2})
	end,
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
