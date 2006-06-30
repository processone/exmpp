% $Id$

-module(check_xml_element_handlers).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_get_element_by_name2(),
	test_get_element_by_name3(),
	test_append_child(),
	ok.

% --------------------------------------------------------------------
% Element handlers testsuite.
% --------------------------------------------------------------------

-define(XML_NS, 'http://www.w3.org/XML/1998/namespace').

-define(TARGET, {xmlelement, "target",
	[],
	[]}
).

-define(ELEMENT1, {xmlelement, "element",
	[],
	[]}
).

-define(ELEMENT2, {xmlelement, "element",
	[],
	[?TARGET]}
).

-define(ELEMENT3, {xmlelement, "element",
	[],
	[?ELEMENT2]}
).

-define(TARGET_NS, {xmlnselement,
	?XML_NS, undefined, "target",
	[],
	[]}
).

-define(ELEMENT1_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[]}
).

-define(ELEMENT2_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[?TARGET_NS]}
).

-define(ELEMENT3_NS, {xmlnselement,
	?XML_NS, undefined, "element",
	[],
	[?ELEMENT2_NS]}
).

test_get_element_by_name2() ->
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1,
	    "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2,
	    "target"), ?TARGET),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3,
	    "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
	    "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
	    "target"), ?TARGET_NS),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
	    "target"), false),
	ok.

test_get_element_by_name3() ->
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
	    ?XML_NS, "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
	    ?XML_NS, "target"), ?TARGET_NS),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
	    ?XML_NS, "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
	    'some_other_ns', "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
	    'some_other_ns', "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
	    'some_other_ns', "target"), false),
	ok.

test_append_child() ->
	testsuite:is(exmpp_xml:append_child(?ELEMENT1, ?TARGET),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:append_child(?ELEMENT1_NS, ?TARGET_NS),
	    ?ELEMENT2_NS),
	ok.
