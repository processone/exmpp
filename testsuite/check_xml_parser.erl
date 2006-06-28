% $Id$

-module(check_xml_parser).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0]).

check() ->
	testsuite:ok(test_parser_without_option()),
	testsuite:ok(test_parser_with_atom()),
	testsuite:ok(test_parser_with_namespace()),
	testsuite:ok(test_parser_with_namespace_and_atom()),
	testsuite:ok(test_parser_with_data_inf_maxsize()),
	testsuite:ok(test_parser_with_data_sup_maxsize()),
	testsuite:ok(test_parser_with_root_depth()),
	testsuite:ok(test_parser_with_ns_root_depth()),
	testsuite:ok(test_parser_with_end_element()),
	testsuite:ok(test_parser_with_ns_end_element()),
	testsuite:pass().

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
{xmlnselement, 'ns_stream', undefined, "stream", [], [
	{xmlnselement, 'ns_default', undefined, "iq", [
		{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
		    "lang", "fr"}
	], [
		{xmlcdata, <<"Content">>}
	]}
]}
]).

-define(TREE1_NS_ATOM, [
{xmlnselement, 'ns_stream', undefined, 'stream', [], [
	{xmlnselement, 'ns_default', undefined, 'iq', [
		{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
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
{xmlnselement, 'ns_stream', undefined, "stream", [], []},
{xmlnselement, 'ns_default', undefined, "iq", [
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
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
{xmlnselement, 'ns_stream', undefined, "stream", [], []},
{xmlnselement, 'ns_default', undefined, "iq", [
	{xmlattr, 'http://www.w3.org/XML/1998/namespace', undefined,
	    "lang", "fr"}
], [
	{xmlcdata, <<"Content">>}
]},
{xmlnsendelement, 'ns_stream', "stream"}
]).

test_parser_without_option() ->
	case exmpp_xml:start_parser() of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NO_NS} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NO_NS}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_atom() ->
	case exmpp_xml:start_parser([name_as_atom]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NO_NS_ATOM} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NO_NS_ATOM}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_namespace() ->
	case exmpp_xml:start_parser([namespace]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NS} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NS}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_namespace_and_atom() ->
	case exmpp_xml:start_parser([namespace, name_as_atom]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NS_ATOM} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NS_ATOM}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_data_inf_maxsize() ->
	case exmpp_xml:start_parser([{maxsize, length(?SOURCE1)}]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NO_NS} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NO_NS}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_data_sup_maxsize() ->
	case exmpp_xml:start_parser([{maxsize, length(?SOURCE1) - 1}]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, stanza_too_big} ->
					ok;
				Reason ->
					{test_failed, Reason}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_root_depth() ->
	case exmpp_xml:start_parser([{root_depth, 1}]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_ROOT_DEPTH} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_ROOT_DEPTH}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_ns_root_depth() ->
	case exmpp_xml:start_parser([namespace, {root_depth, 1}]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NS_ROOT_DEPTH} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree,
					    ?TREE1_NS_ROOT_DEPTH}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_end_element() ->
	case exmpp_xml:start_parser([{root_depth, 1}, endelement]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_END_EL} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_END_EL}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

test_parser_with_ns_end_element() ->
	case exmpp_xml:start_parser([namespace, {root_depth, 1}, endelement]) of
		{ok, Parser} ->
			Ret = case exmpp_xml:parse_final(Parser, ?SOURCE1) of
				{error, Reason} ->
					{error, Reason};
				{ok, ?TREE1_NS_END_EL} ->
					ok;
				{ok, Tree} ->
					{test_failed, Tree, ?TREE1_NS_END_EL}
			end,
			exmpp_xml:stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

