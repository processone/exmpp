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
	test_append_children(),
	test_prepend_children(),
	test_set_children(),
	test_get_elements_by_name(),
	test_get_child_elements(),
	ok.

% --------------------------------------------------------------------
% Element handlers testsuite.
% --------------------------------------------------------------------

-define(TARGET, {xmlelement, "target",
	[],
	[]}
).

-define(ELEMENT0, {xmlelement, "element",
	[],
	undefined}
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

-define(ELEMENT4, {xmlelement, "element",
	[],
	[?ELEMENT1,?ELEMENT2]}
).

-define(TARGET_NS, {xmlnselement,
	?NS_XML, undefined, "target",
	[],
	[]}
).

-define(ELEMENT0_NS, {xmlnselement,
	?NS_XML, undefined, "element",
	[],
	undefined}
).

-define(ELEMENT1_NS, {xmlnselement,
	?NS_XML, undefined, "element",
	[],
	[]}
).

-define(ELEMENT2_NS, {xmlnselement,
	?NS_XML, undefined, "element",
	[],
	[?TARGET_NS]}
).

-define(ELEMENT3_NS, {xmlnselement,
	?NS_XML, undefined, "element",
	[],
	[?ELEMENT2_NS]}
).

-define(ELEMENT4_NS, {xmlnselement, 
	?NS_XML, undefined, "element",
	[],
	[?ELEMENT1_NS,?ELEMENT2_NS]}
).

test_get_element_by_name2() ->
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT0,
	    "target"), false),
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
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT0_NS,
	    ?NS_XML, "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
	    ?NS_XML, "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
	    ?NS_XML, "target"), ?TARGET_NS),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
	    ?NS_XML, "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
	    'some_other_ns', "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
	    'some_other_ns', "target"), false),
	testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
	    'some_other_ns', "target"), false),
	ok.

test_append_child() ->
	testsuite:is(exmpp_xml:append_child(?ELEMENT0, ?TARGET),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:append_child(?ELEMENT1, ?TARGET),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:append_child(?ELEMENT0_NS, ?TARGET_NS),
	    ?ELEMENT2_NS),
	testsuite:is(exmpp_xml:append_child(?ELEMENT1_NS, ?TARGET_NS),
	    ?ELEMENT2_NS),
	ok.

test_append_children() ->
	testsuite:is(exmpp_xml:append_children(?ELEMENT0, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:append_children(?ELEMENT1, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:append_children(?ELEMENT0_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	testsuite:is(exmpp_xml:append_children(?ELEMENT1_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	ok.

test_prepend_children() ->
	testsuite:is(exmpp_xml:prepend_children(?ELEMENT0, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:prepend_children(?ELEMENT3,[?ELEMENT1]),
	    ?ELEMENT4),
	testsuite:is(exmpp_xml:prepend_children(?ELEMENT0_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	testsuite:is(exmpp_xml:prepend_children(?ELEMENT3_NS,[?ELEMENT1_NS]),
	    ?ELEMENT4_NS),
	ok.
	
test_set_children() ->
	testsuite:is(exmpp_xml:set_children(?ELEMENT0, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:set_children(?ELEMENT1, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:set_children(?ELEMENT2, [?TARGET]),
	    ?ELEMENT2),
	testsuite:is(exmpp_xml:set_children(?ELEMENT0_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	testsuite:is(exmpp_xml:set_children(?ELEMENT1_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	testsuite:is(exmpp_xml:set_children(?ELEMENT2_NS, [?TARGET_NS]),
	    ?ELEMENT2_NS),
	ok.



-define(CHILD_ELEMENT1, {xmlnselement,
	?NS_XML, undefined, "child",
	[],
	[]}
).
-define(CHILD_ELEMENT2, {xmlnselement,
	?NS_XML, undefined, "child",
	[],
	[]}
).
-define(CHILD_ELEMENT3, {xmlnselement,
	?NS_XML, undefined, "child",
	[],
	[]}
).
-define(OTHER_CHILD_ELEMENT, {xmlnselement,
	?NS_XML, undefined, "other",
	[],
	[]}
).
-define(CDATA, {xmlcdata,"some text"}).

-define(ELEMENT5_NS, {xmlnselement,
	?NS_XML, undefined, "element",
	[],
	[?CHILD_ELEMENT1,{},?CHILD_ELEMENT2,?CHILD_ELEMENT3,?OTHER_CHILD_ELEMENT]}
).

test_get_elements_by_name() ->
	testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,"child"),
		[?CHILD_ELEMENT1,?CHILD_ELEMENT2,?CHILD_ELEMENT3]),
	testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,"other"),
		[?OTHER_CHILD_ELEMENT]),
	testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,"non_existent"),
		[]),
	ok.
		
	

test_get_child_elements() ->
	testsuite:is(exmpp_xml:get_child_elements(?ELEMENT5_NS),
		[?CHILD_ELEMENT1,?CHILD_ELEMENT2,?CHILD_ELEMENT3,?OTHER_CHILD_ELEMENT]),
	testsuite:is(exmpp_xml:get_child_elements(?CHILD_ELEMENT3),
		[]),
	ok.

