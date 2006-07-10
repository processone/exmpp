% $Id$

-module(check_xml_attribute_handlers).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	test_get_attribute_node_from_list2(),
	test_get_attribute_node_from_list3(),
	test_get_attribute_node2(),
	test_get_attribute_node3(),
	test_get_attribute_from_list2(),
	test_get_attribute_from_list3(),
	test_get_attribute2(),
	test_get_attribute3(),
	test_set_attribute_in_list3(),
	test_set_attribute_in_list4(),
	test_set_attribute3(),
	test_set_attribute4(),
	test_set_attributes(),
	test_set_attributes_ns(),
	test_has_attribute_in_list2(),
	test_has_attribute_in_list3(),
	test_has_attribute2(),
	test_has_attribute3(),
	test_remove_attribute_from_list2(),
	test_remove_attribute_from_list3(),
	test_remove_attribute2(),
	test_remove_attribute3(),
	ok.

% --------------------------------------------------------------------
% Attribute handlers testsuite.
% --------------------------------------------------------------------

-define(XML_NS, 'http://www.w3.org/XML/1998/namespace').

-define(ATTRIBUTE_LIST1, []).

-define(ATTRIBUTE_LIST2_1, [
	{"version", "1.0"}
]).
-define(ATTRIBUTE_LIST2_2, [
	{"version", "1.0"},
	{"xml:lang", "fr"}
]).

-define(ATTRIBUTE_LIST3_1, [
	{xmlattr, undefined, undefined, "version", "1.0"}
]).
-define(ATTRIBUTE_LIST3_2, [
	{xmlattr, undefined, undefined, "version", "1.0"},
	{xmlattr, ?XML_NS, undefined, "lang", "fr"}
]).

-define(ATTRIBUTE_LIST4, [
	bad_data
]).

-define(ELEMENT1, {xmlnselement,
	undefined, undefined, "element",
	?ATTRIBUTE_LIST1,
	[]}
).

-define(ELEMENT2_1, {xmlelement,
	"element",
	?ATTRIBUTE_LIST2_1,
	[]}
).

-define(ELEMENT2_2, {xmlelement,
	"element",
	?ATTRIBUTE_LIST2_2,
	[]}
).

-define(ELEMENT3_1, {xmlnselement,
	undefined, undefined, "element",
	?ATTRIBUTE_LIST3_1,
	[]}
).

-define(ELEMENT3_2, {xmlnselement,
	undefined, undefined, "element",
	?ATTRIBUTE_LIST3_2,
	[]}
).

-define(ELEMENT4, {xmlnselement,
	undefined, undefined, "bad_element",
	?ATTRIBUTE_LIST4,
	[]}
).

-define(ELEMENT5, {xmlelement,
	"element",
	[],
	[
		?ELEMENT1
	]}
).

-define(ELEMENT6, {xmlnselement,
	undefined, undefined, "element",
	[],
	[
		{cdata, <<"Content 1">>}
	]}
).

-define(ELEMENT7, {xmlnselement,
	undefined, undefined, "element",
	[],
	[
		?ELEMENT1,
		{cdata, <<"Content 1">>},
		?ELEMENT1
	]}
).

test_get_attribute_node_from_list2() ->
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST1, "xml:lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST2_1, "xml:lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST2_2, "xml:lang"),
	    {"xml:lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST3_1, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST3_2, "lang"),
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST4, "lang"),
	    false),
	ok.

test_get_attribute_node_from_list3() ->
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST1, ?XML_NS, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST3_1, ?XML_NS, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST3_2, ?XML_NS, "lang"),
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node_from_list(
	    ?ATTRIBUTE_LIST4, ?XML_NS, "lang"),
	    false),
	ok.

test_get_attribute_node2() ->
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT1, "xml:lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT2_1, "xml:lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT2_2, "xml:lang"),
	    {"xml:lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT3_1, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT3_2, "lang"),
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT4, "lang"),
	    false),
	ok.

test_get_attribute_node3() ->
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT1, ?XML_NS, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT3_1, ?XML_NS, "lang"),
	    false),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT3_2, ?XML_NS, "lang"),
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}),
	testsuite:is(exmpp_xml:get_attribute_node(
	    ?ELEMENT4, ?XML_NS, "lang"),
	    false),
	ok.

test_get_attribute_from_list2() ->
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST1, "xml:lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST2_1, "xml:lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST2_2, "xml:lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST3_1, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST3_2, "lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST4, "lang"),
	    ""),
	ok.

test_get_attribute_from_list3() ->
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST1, ?XML_NS, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST3_1, ?XML_NS, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST3_2, ?XML_NS, "lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute_from_list(
	    ?ATTRIBUTE_LIST4, ?XML_NS, "lang"),
	    ""),
	ok.

test_get_attribute2() ->
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT1, "xml:lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT2_1, "xml:lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT2_2, "xml:lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT3_1, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT3_2, "lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT4, "lang"),
	    ""),
	ok.

test_get_attribute3() ->
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT1, ?XML_NS, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT3_1, ?XML_NS, "lang"),
	    ""),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT3_2, ?XML_NS, "lang"),
	    "fr"),
	testsuite:is(exmpp_xml:get_attribute(
	    ?ELEMENT4, ?XML_NS, "lang"),
	    ""),
	ok.

test_has_attribute_in_list2() ->
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST1, "xml:lang"), false),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST2_1, "xml:lang"), false),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST2_2, "xml:lang"), true),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST3_1, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST3_2, "lang"), true),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST4, "lang"), false),
	ok.

test_has_attribute_in_list3() ->
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST1, ?XML_NS, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST3_1, ?XML_NS, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST3_2, ?XML_NS, "lang"), true),
	testsuite:is(exmpp_xml:has_attribute_in_list(
	    ?ATTRIBUTE_LIST4, ?XML_NS, "lang"), false),
	ok.

test_has_attribute2() ->
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT1, "xml:lang"), false),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT2_1, "xml:lang"), false),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT2_2, "xml:lang"), true),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT3_1, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT3_2, "lang"), true),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT4, "lang"), false),
	ok.

test_has_attribute3() ->
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT1, ?XML_NS, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT3_1, ?XML_NS, "lang"), false),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT3_2, ?XML_NS, "lang"), true),
	testsuite:is(exmpp_xml:has_attribute(
	    ?ELEMENT4, ?XML_NS, "lang"), false),
	ok.

test_set_attribute_in_list3() ->
	New1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST1,
	    "lang", "en"),
	testsuite:is(New1, [
	    {xmlattr, undefined, undefined, "lang", "en"}
	]),
	New2_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST2_1,
	    "xml:lang", "en"),
	testsuite:is(New2_1, [
	    {"version", "1.0"},
	    {"xml:lang","en"}
	]),
	New2_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST2_2,
	    "xml:lang", "en"),
	testsuite:is(New2_2, [
	    {"version", "1.0"},
	    {"xml:lang","en"}
	]),
	New3_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_1,
	    "lang", "en"),
	testsuite:is(New3_1, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, undefined, undefined, "lang", "en"}
	]),
	New3_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
	    "lang", "en"),
	testsuite:is(New3_2, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	]),
	New4 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST4,
	    "lang", "en"),
	testsuite:is(New4, [
	    bad_data,
	    {xmlattr, undefined, undefined, "lang", "en"}
	]),
	ok.

test_set_attribute_in_list4() ->
	New1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST1,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New1, [
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	]),
	New3_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_1,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New3_1, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	]),
	New3_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New3_2, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	]),
	New3_3 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
	    'some_other_ns', "lang", "en"),
	testsuite:is(New3_3, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"},
	    {xmlattr, 'some_other_ns', undefined, "lang", "en"}
	]),
	New4 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST4,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New4, [
	    bad_data,
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	]),
	ok.

test_set_attribute3() ->
	New1 = exmpp_xml:set_attribute(?ELEMENT1,
	    "lang", "en"),
	testsuite:is(New1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "lang", "en"}
	    ], []
	}),
	New2_1 = exmpp_xml:set_attribute(?ELEMENT2_1,
	    "xml:lang", "en"),
	testsuite:is(New2_1, {xmlelement,
	    "element", [
	    {"version", "1.0"},
	    {"xml:lang","en"}
	    ], []
	}),
	New2_2 = exmpp_xml:set_attribute(?ELEMENT2_2,
	    "xml:lang", "en"),
	testsuite:is(New2_2, {xmlelement,
	    "element", [
	    {"version", "1.0"},
	    {"xml:lang","en"}
	    ], []
	}),
	New3_1 = exmpp_xml:set_attribute(?ELEMENT3_1,
	    "lang", "en"),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, undefined, undefined, "lang", "en"}
	    ], []
	}),
	New3_2 = exmpp_xml:set_attribute(?ELEMENT3_2,
	    "lang", "en"),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	New4 = exmpp_xml:set_attribute(?ELEMENT4,
	    "lang", "en"),
	testsuite:is(New4, {xmlnselement,
	    undefined, undefined, "bad_element", [
	    bad_data,
	    {xmlattr, undefined, undefined, "lang", "en"}
	    ], []
	}),
	ok.

test_set_attribute4() ->
	New1 = exmpp_xml:set_attribute(?ELEMENT1,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	New3_1 = exmpp_xml:set_attribute(?ELEMENT3_1,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	New3_2 = exmpp_xml:set_attribute(?ELEMENT3_2,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	New3_3 = exmpp_xml:set_attribute(?ELEMENT3_2,
	    'some_other_ns', "lang", "en"),
	testsuite:is(New3_3, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"},
	    {xmlattr, 'some_other_ns', undefined, "lang", "en"}
	    ], []
	}),
	New4 = exmpp_xml:set_attribute(?ELEMENT4,
	    ?XML_NS, "lang", "en"),
	testsuite:is(New4, {xmlnselement,
	    undefined, undefined, "bad_element", [
	    bad_data,
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	ok.

test_set_attributes() ->
	New1 = exmpp_xml:set_attributes(?ELEMENT1, []),
	testsuite:is(New1, ?ELEMENT1),
	New3_1 = exmpp_xml:set_attributes(?ELEMENT3_1,
	    [{"version", "2.0"}, {"lang", "en"}]),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "2.0"},
	    {xmlattr, undefined, undefined, "lang", "en"}
	    ], []
	}),
	New3_2 = exmpp_xml:set_attributes(?ELEMENT3_2,
	    [{"version", "2.0"}, {"lang", "en"}]),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "2.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	ok.

test_set_attributes_ns() ->
	New3_1 = exmpp_xml:set_attributes(?ELEMENT3_1,
	    [{undefined, "version", "2.0"}, {?XML_NS, "lang", "en"}]),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "2.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	New3_2 = exmpp_xml:set_attributes(?ELEMENT3_2,
	    [{undefined, "version", "2.0"}, {?XML_NS, "lang", "en"}]),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "2.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "en"}
	    ], []
	}),
	ok.

test_remove_attribute_from_list2() ->
	New1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST1,
	    "lang"),
	testsuite:is(New1, []),
	New2_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST2_1,
	    "xml:lang"),
	testsuite:is(New2_1, [
	    {"version", "1.0"}
	]),
	New2_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST2_2,
	    "xml:lang"),
	testsuite:is(New2_2, [
	    {"version", "1.0"}
	]),
	New3_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_1,
	    "lang"),
	testsuite:is(New3_1, [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	]),
	New3_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
	    "lang"),
	testsuite:is(New3_2, [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	]),
	New4 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST4,
	    "lang"),
	testsuite:is(New4, [
	    bad_data
	]),
	ok.

test_remove_attribute_from_list3() ->
	New1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST1,
	    ?XML_NS, "lang"),
	testsuite:is(New1, []),
	New3_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_1,
	    ?XML_NS, "lang"),
	testsuite:is(New3_1, [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	]),
	New3_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
	    ?XML_NS, "lang"),
	testsuite:is(New3_2, [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	]),
	New3_3 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
	    'some_other_ns', "lang"),
	testsuite:is(New3_3, [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}
	]),
	New4 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST4,
	    ?XML_NS, "lang"),
	testsuite:is(New4, [
	    bad_data
	]),
	ok.

test_remove_attribute2() ->
	New1 = exmpp_xml:remove_attribute(?ELEMENT1,
	    "lang"),
	testsuite:is(New1, {xmlnselement,
	    undefined, undefined, "element", [
	    ], []
	}),
	New2_1 = exmpp_xml:remove_attribute(?ELEMENT2_1,
	    "xml:lang"),
	testsuite:is(New2_1, {xmlelement,
	    "element", [
	    {"version", "1.0"}
	    ], []
	}),
	New2_2 = exmpp_xml:remove_attribute(?ELEMENT2_2,
	    "xml:lang"),
	testsuite:is(New2_2, {xmlelement,
	    "element", [
	    {"version", "1.0"}
	    ], []
	}),
	New3_1 = exmpp_xml:remove_attribute(?ELEMENT3_1,
	    "lang"),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	    ], []
	}),
	New3_2 = exmpp_xml:remove_attribute(?ELEMENT3_2,
	    "lang"),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	    ], []
	}),
	New4 = exmpp_xml:remove_attribute(?ELEMENT4,
	    "lang"),
	testsuite:is(New4, {xmlnselement,
	    undefined, undefined, "bad_element", [
	    bad_data
	    ], []
	}),
	ok.

test_remove_attribute3() ->
	New1 = exmpp_xml:remove_attribute(?ELEMENT1,
	    ?XML_NS, "lang"),
	testsuite:is(New1, {xmlnselement,
	    undefined, undefined, "element", [
	    ], []
	}),
	New3_1 = exmpp_xml:remove_attribute(?ELEMENT3_1,
	    ?XML_NS, "lang"),
	testsuite:is(New3_1, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	    ], []
	}),
	New3_2 = exmpp_xml:remove_attribute(?ELEMENT3_2,
	    ?XML_NS, "lang"),
	testsuite:is(New3_2, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"}
	    ], []
	}),
	New3_3 = exmpp_xml:remove_attribute(?ELEMENT3_2,
	    'some_other_ns', "lang"),
	testsuite:is(New3_3, {xmlnselement,
	    undefined, undefined, "element", [
	    {xmlattr, undefined, undefined, "version", "1.0"},
	    {xmlattr, ?XML_NS, undefined, "lang", "fr"}
	    ], []
	}),
	New4 = exmpp_xml:remove_attribute(?ELEMENT4,
	    ?XML_NS, "lang"),
	testsuite:is(New4, {xmlnselement,
	    undefined, undefined, "bad_element", [
	    bad_data
	    ], []
	}),
	ok.
