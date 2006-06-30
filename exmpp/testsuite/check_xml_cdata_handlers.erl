% $Id$

-module(check_xml_cdata_handlers).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_get_cdata_from_list(),
	test_get_cdata(),
	test_remove_cdata_from_list(),
	test_remove_cdata(),
	ok.

% --------------------------------------------------------------------
% CDATA handlers testsuite.
% --------------------------------------------------------------------

-define(XML_NS, 'http://www.w3.org/XML/1998/namespace').

-define(CDATA, {xmlcdata, <<"Content">>}).

-define(ELEMENT1, {xmlelement, "element",
	[],
	[]}
).

-define(ELEMENT2, {xmlelement, "element",
	[],
	[?CDATA]}
).

-define(ELEMENT3, {xmlelement, "element",
	[],
	[?ELEMENT2]}
).

-define(ELEMENT1_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[]}
).

-define(ELEMENT2_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[?CDATA]}
).

-define(ELEMENT3_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[?ELEMENT2_NS]}
).

test_get_cdata_from_list() ->
	testsuite:is(exmpp_xml:get_cdata_from_list([]),
	    ""),
	testsuite:is(exmpp_xml:get_cdata_from_list([?CDATA]),
	    "Content"),
	testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT1]),
	    ""),
	testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT1, ?CDATA]),
	    "Content"),
	ok.

test_get_cdata() ->
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT1),
	    ""),
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT2),
	    "Content"),
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT3),
	    ""),
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT1_NS),
	    ""),
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT2_NS),
	    "Content"),
	testsuite:is(exmpp_xml:get_cdata(?ELEMENT3_NS),
	    ""),
	ok.

test_remove_cdata_from_list() ->
	testsuite:is(exmpp_xml:remove_cdata_from_list([]),
	    []),
	testsuite:is(exmpp_xml:remove_cdata_from_list([?CDATA]),
	    []),
	testsuite:is(exmpp_xml:remove_cdata_from_list([?ELEMENT1]),
	    [?ELEMENT1]),
	testsuite:is(exmpp_xml:remove_cdata_from_list([?ELEMENT1, ?CDATA]),
	    [?ELEMENT1]),
	ok.

test_remove_cdata() ->
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT1),
	    ?ELEMENT1),
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT2),
	    ?ELEMENT1),
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT3),
	    ?ELEMENT3),
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT1_NS),
	    ?ELEMENT1_NS),
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT2_NS),
	    ?ELEMENT1_NS),
	testsuite:is(exmpp_xml:remove_cdata(?ELEMENT3_NS),
	    ?ELEMENT3_NS),
	ok.
