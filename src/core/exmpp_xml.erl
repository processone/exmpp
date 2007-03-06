% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> is an XML parser based on Expat.
%%
%% <p>
%% It provides a set of functions to prepare a tree of the elements from
%% an XML stream or an XML document. To ease the handling of the tree
%% produced by the parsing, it also export functions to access each
%% parts of an element.
%% </p>
%%
%% <p>
%% Namespace support is fully tested and is now ready for production use.
%% </p>
%%
%% <p>
%% A backward compatible layer, built on top of this module, is provided
%% by {@link xml}.
%% </p>

-module(exmpp_xml).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
	start_parser/0,
	start_parser/1,
	stop_parser/1,
	port_revision/1,
	parse/2,
	parse_final/2,
	parse_document/1,
	parse_document/2,
	parse_document_fragment/1,
	parse_document_fragment/2
]).
-export([
	get_attribute_node_from_list/2,
	get_attribute_node_from_list/3,
	get_attribute_node/2,
	get_attribute_node/3,
	get_attribute_from_list/2,
	get_attribute_from_list/3,
	get_attribute/2,
	get_attribute/3,
	set_attribute_in_list/3,
	set_attribute_in_list/4,
	set_attribute/3,
	set_attribute/4,
	set_attributes/2,
	has_attribute_in_list/2,
	has_attribute_in_list/3,
	has_attribute/2,
	has_attribute/3,
	remove_attribute_from_list/2,
	remove_attribute_from_list/3,
	remove_attribute/2,
	remove_attribute/3
]).
-export([
	get_element_by_name/2,
	get_element_by_name/3,
	get_element_by_ns/2,
	append_child/2,
	append_children/2,
	replace_child/3,
	set_children/2
]).
-export([
	get_cdata_from_list/1,
	get_cdata/1,
	normalize_cdata_in_list/1,
	normalize_cdata/1,
	set_cdata_in_list/2,
	set_cdata/2,
	remove_cdata_from_list/1,
	remove_cdata/1
]).
-export([
	get_path/2,
	xmlnselement_to_xmlelement/1,
	xmlnselement_to_xmlelement/3,
	xmlelement_to_xmlnselement/1,
	xmlelement_to_xmlnselement/3,
	xmlelement_to_xmlnselement_and_ns_tables/3,
	document_fragment_to_list/3,
	document_to_list/1,
	clear_endelement_tuples/1,
	encode_entities/1
]).

-record(xml_parser, {
	port
}).

-define(DRIVER_NAME, exmpp_expat_drv).

-define(EXPAT_SET_NSPARSER,     1).
-define(EXPAT_SET_NAMEASATOM,   2).
-define(EXPAT_SET_CHECK_NS,     3).
-define(EXPAT_SET_CHECK_NAMES,  4).
-define(EXPAT_SET_CHECK_ATTRS,  5).
-define(EXPAT_SET_MAXSIZE,      6).
-define(EXPAT_SET_ROOTDEPTH,    7).
-define(EXPAT_SET_ENDELEMENT,   8).
-define(EXPAT_PARSE,            9).
-define(EXPAT_PARSE_FINAL,     10).
-define(EXPAT_SVN_REVISION,    11).

-define(DEFAULT_PARSER_OPTIONS, [
	% no_namespace, % Handled by start_parser/1.
	name_as_string,
	% ns_check,     % By default in the port driver.
	% names_check,
	% attrs_check,
	no_endelement,
	{root_depth, 0},
	no_maxsize
]).

-define(PREFIXED_NAME(P, N), P ++ ":" ++ N).

-define(IMPLICIT_NAMESPACES, [
	{?NS_XML, "xml"}
]).

% --------------------------------------------------------------------
% Parsing functions (interface to the Expat port driver).
% --------------------------------------------------------------------

%% @spec () -> {ok, Parser} | {error, Reason}
%%     Parser = xmlparser()
%% @doc Initialize the Expart port driver with default options.
%%
%% Default options are:
%% ```
%% [no_namespace, name_as_string, no_endelement, {root_depth, 0}, no_maxsize]
%% '''
%%
%% Activating namespace support enables `ns_check'. Activating
%% `name_as_atom' enables `names_check' and `attrs_check'.
%%
%% @see start_parser/1.
%% @see xmlparseroption().

start_parser() ->
	start_parser([]).

%% @spec (Options) -> {ok, Parser} | {error, Reason}
%%     Parser = xmlparser()
%%     Options = [xmlparseroption()]
%% @doc Initialize the Expat port driver with given `Options'.
%%
%% You must call this function before any use of functions {@link
%% parse/2} or {@link parse_final/2}. The returned `Parser' must be
%% given as the first argument for those functions. When finished, you
%% must free this parser with the {@link stop_parser/1}. Here is an
%% example:
%% ```
%% fun() ->
%%     {ok, Parser} = xml:start_parser(),
%%     xml:parse(Parser, "<stream version='1.0'><presence/></stream>"),
%%     xml:stop_parser(Parser).
%% '''

start_parser(Options) ->
	case load_driver() of
		{error, Reason} ->
			{error, Reason};
		Port ->
			Parser = #xml_parser{port = Port},
			Options2 = case lists:member(no_namespace, Options) of
				true ->
					Options;
				false ->
					case lists:member(namespace, Options) of
						true ->
							Options;
						false ->
							[no_namespace | Options]
					end
			end,
			case handle_options(
			    ?DEFAULT_PARSER_OPTIONS ++ Options2, Parser) of
				{error, Reason} ->
					unload_driver(),
					{error, Reason};
				New_Parser ->
					{ok, New_Parser}
			end
	end.

%% @spec (Parser) -> true
%%     Parser = xmlparser()
%% @doc Stop the Expat port driver.
%%
%% This must be called when you are done with the `Parser' returned by
%% {@link start_parser/0}.
%%
%% @see start_parser/0. `start_parser/0' for an example

stop_parser(#xml_parser{port = Port} = _Parser) ->
	port_close(Port),
	unload_driver().

%% @spec (Parser, Data) -> {ok, [XML_Element]} | {ok, continue} | {error, Reason}
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlelement() | xmlnselement() | xmlendelement() | xmlnsendelement()
%% @doc Parse a chunk from an XML stream.
%%
%% This may be called multiple times with a new chunk of data. However
%% the entire data must represent at most one and only one XML document.
%% If you want to process the last chunk of data, you should call {@link
%% parser_final/2}. If you can't know when the end of the document
%% occurs, you may use this function to process data, then you call
%% {@link parse_final/2} with an empty string. Here is an example:
%% ```
%% fun (Parser) ->
%%     xml:parse(Parser, "<stream ver"),
%%     xml:parse(Parser, "sion='1."),
%%     xml:parse(Parser, "0'></stream>"),
%%     xml:parser_final(Parser, "").
%% '''

parse(Parser, Data) when is_list(Data) ->
	parse(Parser, list_to_binary(Data));

parse(#xml_parser{port = Port} = _Parser, Data) when is_binary(Data) ->
	binary_to_term(port_control(Port, ?EXPAT_PARSE, Data)).

%% @spec (Parser, Data) -> {ok, [XML_Element]} | {ok, done} | {error, Reason}
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlelement() | xmlnselement() | xmlendelement() | xmlnsendelement()
%% @doc Parse the last chunk from an XML stream.
%%
%% This is used when you know there won't be any more data to process.
%% This last chunk must provide the end of the XML document or the
%% parser will return an error. This function may also be used to
%% process an entire XML document in one pass.
%%
%% @see parse/2. `parse/2' for an example

parse_final(Parser, Data) when is_list(Data) ->
	parse_final(Parser, list_to_binary(Data));

parse_final(#xml_parser{port = Port} = _Parser, Data) when is_binary(Data) ->
	binary_to_term(port_control(Port, ?EXPAT_PARSE_FINAL, Data)).

%% @spec (Document) -> {ok, [XML_Element]} | {error, Reason}
%%     Document = string() | binary()
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%%     Reason = term()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options; see
%% {@link start_parser/1} for any related informations.

parse_document(Document) ->
	parse_document(Document, []).

%% @spec (Document, Parser_Options) -> {ok, [XML_Element]} | {error, Reason}
%%     Document = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%%     Reason = term()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser; see {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

parse_document(Document, Parser_Options) ->
	case start_parser(Parser_Options) of
		{ok, Parser} ->
			Ret = parse_final(Parser, Document),
			stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

%% @spec (Fragment) -> {ok, [XML_Element]} | {error, Reason}
%%     Fragment = string() | binary()
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%%     Reason = term()
%% @doc Parse a fragment of an XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options,
%% but will set `no_root_depth' (which can be overriden); see {@link
%% start_parser/1} for any related informations.

parse_document_fragment(Fragment) ->
	parse_document_fragment(Fragment, []).

%% @spec (Fragment, Parser_Options) -> {ok, [XML_Element]} | {error, Reason}
%%     Fragment = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%%     Reason = term()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser but `no_root_depth' is prepended (this can be overriden); see
%% {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

parse_document_fragment(Fragment, Parser_Options) ->
	case start_parser([no_root_depth | Parser_Options]) of
		{ok, Parser} ->
			Ret = parse(Parser, Fragment),
			stop_parser(Parser),
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

%% @hidden

port_revision(#xml_parser{port = Port} = _Parser) ->
	binary_to_term(port_control(Port, ?EXPAT_SVN_REVISION, <<>>)).

% --------------------------------------------------------------------
% Functions to handle XML elements (xmlnselement() & xmlelement()).
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (Attrs, Attr_Name) -> Attr | false
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name' from the list.
%%
%% Return `false' if the attribute isn't found.

get_attribute_node_from_list([Attr | Rest], Name) ->
	case Attr of
		#xmlattr{name = Name} ->
			Attr;
		{Name, _Value} ->
			Attr;
		_ ->
			get_attribute_node_from_list(Rest, Name)
	end;
get_attribute_node_from_list([], _Name) ->
	false.

%% @spec (Attrs, NS, Attr_Name) -> Attr | false
%%     Attrs = [xmlnsattribute()]
%%     Attr_Name = string() | atom()
%%     Attr = xmlnsattribute()
%% @doc Return the attribute named `Attr_Name' from the list with the
%% `NS' namespace URI.
%%
%% Return `false' if the attribute isn't found.

get_attribute_node_from_list([Attr | Rest], NS, Name) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			Attr;
		_ ->
			get_attribute_node_from_list(Rest, NS, Name)
	end;
get_attribute_node_from_list([], _NS, _Name) ->
	false.

%% @spec (XML_Element, Attr_Name) -> Attr | false
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attr_Name = string() | atom()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name'.
%%
%% Return `false' if the attribute isn't found.

get_attribute_node(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
	get_attribute_node_from_list(Attrs, Name);

get_attribute_node(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
	get_attribute_node_from_list(Attrs, Name).

%% @spec (XML_Element, NS, Attr_Name) -> Attr | false
%%     XML_Element = xmlnselement()
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     Attr = xmlnsattribute()
%% @doc Return the attribute named `Attr_Name' with the `NS' namespace URI.
%%
%% Return `false' if the attribute isn't found.

get_attribute_node(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
	get_attribute_node_from_list(Attrs, NS, Name).

%% @spec (Attrs, Attr_Name) -> Attr_Value | nil()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' from the list.
%%
%% Return an empty string if the attribute isn't found.

get_attribute_from_list(Attrs, Attr_Name) ->
	case get_attribute_node_from_list(Attrs, Attr_Name) of
		#xmlattr{value = Value} ->
			Value;
		{_Name, Value} ->
			Value;
		_ ->
			""
	end.

%% @spec (Attrs, NS, Attr_Name) -> Attr_Value | nil()
%%     Attrs = [xmlnsattribute()]
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' from the
%% list with the `NS' namespace URI.
%%
%% Return an empty string if the attribute isn't found.

get_attribute_from_list(Attrs, NS, Attr_Name) ->
	case get_attribute_node_from_list(Attrs, NS, Attr_Name) of
		#xmlattr{value = Value} ->
			Value;
		_ ->
			""
	end.

%% @spec (XML_Element, Attr_Name) -> Attr_Value | nil()
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name'.
%%
%% Return an empty string if the attribute isn't found.

get_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
	get_attribute_from_list(Attrs, Name);

get_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
	get_attribute_from_list(Attrs, Name).

%% @spec (XML_Element, NS, Attr_Name) -> Attr_Value | nil()
%%     XML_Element = xmlnselement()
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI.
%%
%% Return an empty string if the attribute isn't found.

get_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
	get_attribute_from_list(Attrs, NS, Name).

%% @spec (Attrs, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     Attr_Name = strign() | atom()
%% @doc Check the presence of attribute `Attr_Name' in the list.

has_attribute_in_list(Attrs, Name) ->
	case get_attribute_node_from_list(Attrs, Name) of
		false -> false;
		_     -> true
	end.

%% @spec (Attrs, NS, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     NS = atom()
%%     Attr_Name = strign() | atom()
%% @doc Check the presence of attribute `Attr_Name' with namespace `NS'
%% in the list.

has_attribute_in_list(Attrs, NS, Name) ->
	case get_attribute_node_from_list(Attrs, NS, Name) of
		false -> false;
		_     -> true
	end.

%% @spec (XML_Element, Attr_Name) -> bool()
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attr_Name = strign() | atom()
%% @doc Check the presence of attribute `Attr_Name' in the XML element.

has_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
	has_attribute_in_list(Attrs, Name);

has_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
	has_attribute_in_list(Attrs, Name).

%% @spec (XML_Element, NS, Attr_Name) -> bool()
%%     XML_Element = xmlnselement() | xmlelement()
%%     NS = atom()
%%     Attr_Name = strign() | atom()
%% @doc Check the presence of attribute `Attr_Name' with namespace `NS'
%% in the XML element.

has_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
	has_attribute_in_list(Attrs, NS, Name).

%% @spec (Attrs, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlnsattribute()} record if it can't determine the type from the
%% other attributes.

set_attribute_in_list(Attrs, Name, Value) ->
	set_attribute_in_list2(Attrs, Name, Value, []).

set_attribute_in_list2([Attr | Rest], Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		{Name, _Value} ->
			New_Attr = {Name, Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_in_list2(Rest, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_in_list2([], Name, Value, New_Attrs) ->
	New_Attr = case New_Attrs of
		[#xmlattr{} | _] ->
			#xmlattr{name = Name, value = Value};
		[{_, _} | _] ->
			{Name, Value};
		_ ->
			#xmlattr{name = Name, value = Value}
	end,
	New_Attrs ++ [New_Attr].

%% @spec (Attrs, NS, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlnsattribute()]
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%%     New_Attrs = [xmlnsattribute()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlnsattribute()} record.

set_attribute_in_list(Attrs, NS, Name, Value) ->
	set_attribute_in_list2(Attrs, NS, Name, Value, []).

set_attribute_in_list2([Attr | Rest], NS, Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_in_list2(Rest, NS, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_in_list2([], NS, Name, Value, New_Attrs) ->
	New_Attrs ++ [#xmlattr{ns = NS, name = Name, value = Value}].

%% @spec (XML_Element, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = [xmlnselement() | xmlelement()]
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Add a new attribute or change the value of an existing attribute.

set_attribute(#xmlnselement{attrs = Attrs} = XML_Element, Name, Value) ->
	New_Attrs = set_attribute_ns2(Attrs, Name, Value, []),
	XML_Element#xmlnselement{attrs = New_Attrs};

set_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name, Value) ->
	New_Attrs = set_attribute2(Attrs, Name, Value, []),
	XML_Element#xmlelement{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_ns2(Rest, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_ns2([], Name, Value, New_Attrs) ->
	New_Attrs ++ [#xmlattr{name = Name, value = Value}].

set_attribute2([Attr | Rest], Name, Value, New_Attrs) ->
	case Attr of
		{Name, _Value} ->
			New_Attr = {Name, Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute2(Rest, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute2([], Name, Value, New_Attrs) ->
	New_Attrs ++ [{Name, Value}].

%% @spec (XML_Element, NS, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.

set_attribute(#xmlnselement{attrs = Attrs} = XML_Element, NS, Name, Value) ->
	New_Attrs = set_attribute_ns2(Attrs, NS, Name, Value, []),
	XML_Element#xmlnselement{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], NS, Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_ns2(Rest, NS, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_ns2([], NS, Name, Value, New_Attrs) ->
	New_Attrs ++ [#xmlattr{ns = NS, name = Name, value = Value}].

%% @spec (XML_Element, Attrs_Spec) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attrs_Spec = [{Name, Value} | {NS, Name, Value}]
%%       NS = atom()
%%       Name = string() | atom()
%%       Value = string()
%%     New_XML_Element = xmlnselement() | xmlelement()

set_attributes(XML_Element, [{Name, Value} | Rest]) ->
	New_XML_Element = set_attribute(XML_Element, Name, Value),
	set_attributes(New_XML_Element, Rest);

set_attributes(XML_Element, [{NS, Name, Value} | Rest]) ->
	New_XML_Element = set_attribute(XML_Element, NS, Name, Value),
	set_attributes(New_XML_Element, Rest);

set_attributes(XML_Element, []) ->
	XML_Element.

%% @spec (Attrs, Attr_Name) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Remove attribute named `Attr_Name' and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute_from_list(Attrs, Name) ->
	remove_attribute_from_list2(Attrs, Name, []).

remove_attribute_from_list2([Attr | Rest], Name, New_Attrs) ->
	case Attr of
		#xmlattr{name = Name} ->
			lists:reverse(New_Attrs) ++ Rest;
		{Name, _Value} ->
			lists:reverse(New_Attrs) ++ Rest;
		_ ->
			remove_attribute_from_list2(Rest, Name,
			    [Attr | New_Attrs])
	end;
remove_attribute_from_list2([], _Name, New_Attrs) ->
	lists:reverse(New_Attrs).

%% @spec (Attrs, NS, Attr_Name) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute_from_list(Attrs, NS, Name) ->
	remove_attribute_from_list2(Attrs, NS, Name, []).

remove_attribute_from_list2([Attr | Rest], NS, Name, New_Attrs) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			lists:reverse(New_Attrs) ++ Rest;
		_ ->
			remove_attribute_from_list2(Rest, NS, Name,
			    [Attr | New_Attrs])
	end;
remove_attribute_from_list2([], _NS, _Name, New_Attrs) ->
	lists:reverse(New_Attrs).

%% @spec (XML_Element, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attr_Name = string() | atom()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Remove attribute named `Attr_Name' and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute(#xmlnselement{attrs = Attrs} = XML_Element, Name) ->
	New_Attrs = remove_attribute_from_list(Attrs, Name),
	XML_Element#xmlnselement{attrs = New_Attrs};

remove_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name) ->
	New_Attrs = remove_attribute_from_list(Attrs, Name),
	XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (XML_Element, NS, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlnselement()
%%     NS = atom()
%%     Attr_Name = string() | atom()
%%     New_XML_Element = xmlnselement()
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute(#xmlnselement{attrs = Attrs} = XML_Element, NS, Name) ->
	New_Attrs = remove_attribute_from_list(Attrs, NS, Name),
	XML_Element#xmlnselement{attrs = New_Attrs}.

%% @spec (XML_Element, Name) -> XML_Subelement | false
%%     XML_Element = xmlnselement() | xmlelement()
%%     Name = string() | atom()
%%     XML_Subelement = xmlnselement() | xmlelement()
%% @doc Search in the children of `XML_Element' an element named `Name'.
%%
%% If no element with the given name is found, it returns `false'.
%% This will only search among direct children.

get_element_by_name(#xmlnselement{children = Children}, Name) ->
	get_element_by_name2(Children, Name);

get_element_by_name(#xmlelement{children = Children}, Name) ->
	get_element_by_name2(Children, Name).

get_element_by_name2([Node | Rest], Name) ->
	case Node of
		#xmlnselement{name = Name} ->
			Node;
		#xmlelement{name = Name} ->
			Node;
		_ ->
			get_element_by_name2(Rest, Name)
	end;

get_element_by_name2([], _Name) ->
	false;

get_element_by_name2(undefined, _Name) ->
	false.

%% @spec (XML_Element, NS, Name) -> XML_Subelement | false
%%     XML_Element = xmlnselement()
%%     NS = atom()
%%     Name = string() | atom()
%%     XML_Subelement = xmlnselement()
%% @doc Search in the children of `XML_Element' an element named `Name'
%% with `NS' namespace URI.
%%
%% If no element with the given name is found, it returns `false'.
%% This will only search among direct children.

get_element_by_name(#xmlnselement{children = Children}, NS, Name) ->
	get_element_by_name2(Children, NS, Name).

get_element_by_name2([Node | Rest], NS, Name) ->
	case Node of
		#xmlnselement{ns = NS, name = Name} ->
			Node;
		_ ->
			get_element_by_name2(Rest, NS, Name)
	end;

get_element_by_name2([], _NS, _Name) ->
	false;

get_element_by_name2(undefined, _NS, _Name) ->
	false.

%% @spec (XML_Element, NS) -> XML_Subelement | false
%%     XML_Element = xmlnselement()
%%     NS = atom()
%%     XML_Subelement = xmlnselement()
%% @doc Search in the children of `XML_Element' an element with `NS'
%% namespace URI.
%%
%% If no element with the given namespace is found, it returns
%% `false'. This will only search among direct children.
%%
%% This function is particularly usefull to extract XMPP error codes.

get_element_by_ns(#xmlnselement{children = Children}, NS) ->
	get_element_by_ns2(Children, NS).

get_element_by_ns2([Node | Rest], NS) ->
	case Node of
		#xmlnselement{ns = NS} ->
			Node;
		_ ->
			get_element_by_ns2(Rest, NS)
	end;

get_element_by_ns2([], _NS) ->
	false;

get_element_by_ns2(undefined, _NS) ->
	false.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Child = xmlnselement() | xmlelement() | xmlcdata()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Append `Child' to `XML_Element''s children list.

append_child(#xmlnselement{children = undefined} = XML_Element, Child) ->
	New_Children = [Child],
	XML_Element#xmlnselement{children = New_Children};

append_child(#xmlelement{children = undefined} = XML_Element, Child) ->
	New_Children = [Child],
	XML_Element#xmlelement{children = New_Children};

append_child(#xmlnselement{children = Children} = XML_Element, Child) ->
	New_Children = Children ++ [Child],
	XML_Element#xmlnselement{children = New_Children};

append_child(#xmlelement{children = Children} = XML_Element, Child) ->
	New_Children = Children ++ [Child],
	XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Append every `Children' to `XML_Element''s children list.

append_children(#xmlnselement{children = undefined} = XML_Element,
    New_Children) ->
	XML_Element#xmlnselement{children = New_Children};

append_children(#xmlelement{children = undefined} = XML_Element,
    New_Children) ->
	XML_Element#xmlelement{children = New_Children};

append_children(#xmlnselement{children = Children} = XML_Element,
    New_Children) ->
	Concat_Children = Children ++ New_Children,
	XML_Element#xmlnselement{children = Concat_Children};

append_children(#xmlelement{children = Children} = XML_Element,
    New_Children) ->
	Concat_Children = Children ++ New_Children,
	XML_Element#xmlelement{children = Concat_Children}.

%% @spec (XML_Element, Old_Child, New_Child) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Old_Child = xmlnselement() | xmlelement()
%%     New_Child = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Replace `Old_Child' by `New_Child' in `XML_Element' children list.

replace_child(#xmlnselement{children = Children} = XML_Element,
    Old_Child, New_Child) ->
	New_Children = [case C of
		Old_Child -> New_Child;
		_ -> C
	end || C <- Children],
	XML_Element#xmlnselement{children = New_Children};

replace_child(#xmlelement{children = Children} = XML_Element,
    Old_Child, New_Child) ->
	New_Children = [case C of
		Old_Child -> New_Child;
		_ -> C
	end || C <- Children],
	XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Set `XML_Element''s children list to `Children'.
%%
%% Any existing child is removed.

set_children(#xmlnselement{} = XML_Element, New_Children) ->
	XML_Element#xmlnselement{children = New_Children};

set_children(#xmlelement{} = XML_Element, New_Children) ->
	XML_Element#xmlelement{children = New_Children}.

%% @spec (Children) -> CData
%%     Children = undefined | [xmlnselement() | xmlelement() | xmlcdata()]
%%     CData = string()
%% @doc Concatenate and return any character data from th given children list.

get_cdata_from_list(undefined) ->
	"";

get_cdata_from_list(Children) ->
	% The function list_to_binary/1 will concatenate every
	% binaries in the list returned by get_cdata2/2.
	binary_to_list(list_to_binary(get_cdata_from_list2(Children, ""))).

get_cdata_from_list2([#xmlcdata{cdata = Chunk} | Rest], Data) ->
	get_cdata_from_list2(Rest, [Data, Chunk]);
get_cdata_from_list2([_ | Rest], Data) ->
	get_cdata_from_list2(Rest, Data);
get_cdata_from_list2([], Data) ->
	Data.

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlnselement() | xmlelement()
%%     CData = string()
%% @doc Concatenate and return any character data of the given XML element.
%%
%% This function is `get_tag_cdata/1' renamed in `get_cdata/1'. It
%% doesn't take a list of children like the old `get_cdata/1', use
%% {@link get_cdata_from_list/1} for this purpose!

get_cdata(#xmlnselement{children = Children}) ->
	get_cdata_from_list(Children);

get_cdata(#xmlelement{children = Children}) ->
	get_cdata_from_list(Children);

get_cdata(false) ->
	% This clause makes it possible to write code like:
	% exmpp_xml:get_cdata(exmpp_xml:get_element_by_name(XML_El, body))
	false.

%% @spec (Children) -> New_Children
%%     Children = undefined | [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_Children = undefined | [xmlnselement() | xmlelement() | xmlcdata()]
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one.
%%
%% One caveats is the reconstructed {@link xmlcdata()} is appended at the end
%% of the children list.

normalize_cdata_in_list(undefined) ->
	undefined;

normalize_cdata_in_list([]) ->
	[];

normalize_cdata_in_list(Children) ->
	CData = get_cdata_from_list(Children),
	Purged_Children = remove_cdata_from_list(Children),
	case CData of
		"" ->
			Purged_Children;
		_ ->
			Purged_Children ++
			[#xmlcdata{cdata = list_to_binary(CData)}]
	end.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one.
%%
%% One caveats is the reconstructed {@link xmlcdata()} is appended at the end
%% of the children list.

normalize_cdata(#xmlnselement{children = Children} = XML_Element) ->
	New_Children = normalize_cdata_in_list(Children),
	XML_Element#xmlnselement{children = New_Children};

normalize_cdata(#xmlelement{children = Children} = XML_Element) ->
	New_Children = normalize_cdata_in_list(Children),
	XML_Element#xmlelement{children = New_Children}.

%% @spec (Children, CData) -> New_Children
%%     Children = undefined | [xmlnselement() | xmlelement() | xmlcdata()]
%%     CData = string() | binary()
%%     New_Children = [xmlnselement() | xmlelement() | xmlcdata()]
%% @doc Replace any character data by `CData' in the list.
%%
%% The new `CData' is placed at the end of the children list.

set_cdata_in_list(undefined, CData) when is_list(CData) ->
	[#xmlcdata{cdata = list_to_binary(CData)}];

set_cdata_in_list(undefined, CData) ->
	[#xmlcdata{cdata = CData}];

set_cdata_in_list(Children, CData) when is_list(CData) ->
	Purged_Children = remove_cdata_from_list(Children),
	Purged_Children ++ [#xmlcdata{cdata = list_to_binary(CData)}];

set_cdata_in_list(Children, CData) ->
	Purged_Children = remove_cdata_from_list(Children),
	Purged_Children ++ [#xmlcdata{cdata = CData}].

%% @spec (XML_Element, CData) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     CData = string() | binary()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Replace any character data by `CData'.
%%
%% The new `CData' is placed at the end of the children list.

set_cdata(#xmlnselement{children = Children} = XML_Element, CData) ->
	New_Children = set_cdata_in_list(Children, CData),
	XML_Element#xmlnselement{children = New_Children};

set_cdata(#xmlelement{children = Children} = XML_Element, CData) ->
	New_Children = set_cdata_in_list(Children, CData),
	XML_Element#xmlelement{children = New_Children}.

%% @spec (Children) -> New_Children
%%     Children = undefined | [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_Children = undefined | [xmlnselement() | xmlelement()]
%% @doc Remove any character data from the given XML element children list.

remove_cdata_from_list(undefined) ->
	undefined;

remove_cdata_from_list(Children) ->
	[Child || Child <- Children, remove_cdata_from_list2(Child)].

remove_cdata_from_list2({xmlcdata, _CData}) -> false;
remove_cdata_from_list2(_)                  -> true.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Remove any character data from the given XML element.
%%
%% This function doesn't take a list of children like the old
%% `remove_cdata/1', use {@link remove_cdata_from_list/1} for this
%% purpose!

remove_cdata(#xmlnselement{children = Children} = XML_Element) ->
	New_Children = remove_cdata_from_list(Children),
	XML_Element#xmlnselement{children = New_Children};

remove_cdata(#xmlelement{children = Children} = XML_Element) ->
	New_Children = remove_cdata_from_list(Children),
	XML_Element#xmlelement{children = New_Children}.

% --------------------------------------------------------------------
% Function to walk the tree.
% --------------------------------------------------------------------

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlnselement() | xmlelement()
%%     Path = [pathcomponent()]
%%     XML_Subelement = xmlnselement() | xmlelement()
%%     Attr_Value = string()
%%     CData = string()
%%     Not_Found = nil()
%% @doc Follow the given path and return what's pointed by the last
%% component of it.
%%
%% `Path' is a list of path components. If a component points to an
%% {@link xmlnselement()} or {@link xmlelement()}, the function will
%% look for this element and will use it as a base for the next path
%% component. If a component points to an attribute, the function will
%% look for this attribute in the current element and return its value
%% (see {@link get_attribute/2} for the possible return values).
%% If a component asks for character data, the function will return
%% character data for the current element (see {@link get_cdata/1}
%% for possible return values). A path will not be followed further
%% after an attribute or a character data component. If an XML element
%% isn't found while walking through the path, an empty string is
%% returned.

get_path(XML_Element, [{element, Name} | Path]) ->
	case get_element_by_name(XML_Element, Name) of
		false          -> "";
		XML_Subelement -> get_path(XML_Subelement, Path)
	end;
get_path(XML_Element, [{element, NS, Name} | Path]) ->
	case get_element_by_name(XML_Element, NS, Name) of
		false          -> "";
		XML_Subelement -> get_path(XML_Subelement, Path)
	end;
get_path(XML_Element, [{attribute, Name}]) ->
	get_attribute(XML_Element, Name);
get_path(XML_Element, [{attribute, NS, Name}]) ->
	get_attribute(XML_Element, NS, Name);
get_path(XML_Element, [cdata]) ->
	get_cdata(XML_Element);
get_path(XML_Element, []) ->
	XML_Element;
get_path(_XML_Element, [{attribute, _Name} | _Rest]) ->
	{error, ending_component_not_at_the_end};
get_path(_XML_Element, [cdata | _Rest]) ->
	{error, ending_component_not_at_the_end};
get_path(_XML_Element, _Path) ->
	{error, invalid_path}.

% --------------------------------------------------------------------
% Converters.
% --------------------------------------------------------------------

%% @spec (XML_NS_Element) -> XML_Element
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlnselement()} to an {@link xmlelement()} tuple.
%%
%% Other tuples are ignored.

xmlnselement_to_xmlelement(XML_Element) ->
	xmlnselement_to_xmlelement(XML_Element, [], []).

%% @spec (XML_NS_Element, Default_NS, Prefixed_NS) -> XML_Element
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Prefix = string()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlnselement()} to an {@link xmlelement()} tuple.
%%
%% Other tuples are ignored.
%%
%% `Default_NS' and `Prefixed_NS' contain namespace declaration which
%% occured above this fragment in the tree. The order in the first list
%% is important: declarations are sorted from the most recent one to
%% the oldest one.
%%
%% This may be useful in XMPP context where a majority of clients or
%% servers expects a `stream' prefix for the `<stream>' tag and the
%% default namespace declaration in this same element.

xmlnselement_to_xmlelement(
    #xmlnselement{ns = NS, prefix = Prefix, name = Name, attrs = Attrs,
    children = Children}, Default_NS, Prefixed_NS) ->
	{Attrs2, Prefixed_NS2} = xmlnsattributes_to_xmlattributes(
	    Attrs, Prefixed_NS),
	NS_A = if
		is_list(NS)   -> list_to_atom(NS);
		true          -> NS
	end,
	Name_S = if
		is_atom(Name) -> atom_to_list(Name);
		true          -> Name
	end,
	{New_Name, New_Attrs, New_Default_NS,
	    New_Prefixed_NS} = case Default_NS of
		[NS_A | _] ->
			% Use the default namespace.
			{
			    Name_S,
			    Attrs2,
			    Default_NS,
			    Prefixed_NS2
			};
		_ when NS_A /= undefined ->
			case lists:keysearch(NS_A, 1, Prefixed_NS2 ++
			    ?IMPLICIT_NAMESPACES) of
				{value, {_NS_A, Other_Prefix}} ->
					% Use an already declared prefix.
					{
					    ?PREFIXED_NAME(Other_Prefix,
						Name_S),
					    Attrs2,
					    Default_NS,
					    Prefixed_NS2
					};
				false when Prefix /= undefined ->
					% Never declared and provide a new
					% prefix.
					NS_Decl = {"xmlns:" ++ Prefix,
					    atom_to_list(NS_A)},
					{
					    ?PREFIXED_NAME(Prefix,
						Name_S),
					    [NS_Decl | Attrs2],
					    Default_NS,
					    [{NS_A, Prefix} | Prefixed_NS2]
					};
				false ->
					% Never declared and want a DEFAULT
					% namespace.
					NS_Decl = {"xmlns",
					    atom_to_list(NS_A)},
					{
					    Name_S,
					    [NS_Decl | Attrs2],
					    [NS_A | Default_NS],
					    Prefixed_NS2
					}
			end;
		_ ->
			{
			    Name_S,
			    Attrs2,
			    Default_NS,
			    Prefixed_NS2
			}
	end,
	New_Children = xmlnselements_to_xmlelements(Children,
	    New_Default_NS, New_Prefixed_NS),
	#xmlelement{name = New_Name, attrs = New_Attrs,
	    children = New_Children};
xmlnselement_to_xmlelement(#xmlnsendelement{ns = NS, prefix = Prefix,
    name = Name}, Default_NS, Prefixed_NS) ->
	NS_A = if
		is_list(NS)   -> list_to_atom(NS);
		true          -> NS
	end,
	Name_S = if
		is_atom(Name) -> atom_to_list(Name);
		true          -> Name
	end,
	New_Name = case Default_NS of
		[NS_A | _] ->
			Name_S;
		_ when NS_A /= undefined ->
			case lists:keysearch(NS_A, 1, Prefixed_NS ++
			    ?IMPLICIT_NAMESPACES) of
				{value, {_NS_A, Other_Prefix}} ->
					?PREFIXED_NAME(Other_Prefix,
					    Name_S);
				_ when Prefix /= undefined ->
					?PREFIXED_NAME(Prefix,
					    Name_S);
				_ ->
					% Too late to declare something; the
					% namespace should have been provided
					% by the caller.
					Name_S
			end;
		_ ->
			Name_S
	end,
	#xmlendelement{name = New_Name};
xmlnselement_to_xmlelement(XML_El, _Default_NS, _Prefixed_NS) ->
	% xmlelement() or xmlcdata().
	XML_El.

xmlnselements_to_xmlelements(undefined, _Default_NS, _Prefixed_NS) ->
	undefined;
xmlnselements_to_xmlelements([], _Default_NS, _Prefixed_NS) ->
	[];
xmlnselements_to_xmlelements(XML_Elements, Default_NS, Prefixed_NS) ->
	xmlnselements_to_xmlelements2(XML_Elements, [],
	    Default_NS, Prefixed_NS).

xmlnselements_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
    Default_NS, Prefixed_NS) ->
	XML_Element = xmlnselement_to_xmlelement(XML_NS_Element,
	    Default_NS, Prefixed_NS),
	xmlnselements_to_xmlelements2(Rest,
	    [XML_Element | XML_Elements], Default_NS, Prefixed_NS);
xmlnselements_to_xmlelements2([], XML_Elements, _Default_NS, _Prefixed_NS) ->
	lists:reverse(XML_Elements).

xmlnsattributes_to_xmlattributes(Attrs, Prefixed_NS) ->
	xmlnsattributes_to_xmlattributes2(Attrs, Prefixed_NS, []).

xmlnsattributes_to_xmlattributes2(
    [#xmlattr{ns = NS, prefix = Prefix, name = Name, value = Value} | Rest],
    Prefixed_NS, Converted_Attrs) ->
	Name_S = if
		is_atom(Name) -> atom_to_list(Name);
		true          -> Name
	end,
	{New_Name, New_Converted_Attrs, New_Prefixed_NS} = case NS of
		undefined ->
			{
			    Name_S,
			    Converted_Attrs,
			    Prefixed_NS
			};
		_ ->
			case lists:keysearch(NS, 1,
			    Prefixed_NS ++ ?IMPLICIT_NAMESPACES) of
				{value, {_NS, Other_Prefix}} ->
					% Use an already declared prefix.
					{
					    ?PREFIXED_NAME(Other_Prefix,
						Name_S),
					    Converted_Attrs,
					    Prefixed_NS
					};
				false ->
					% Never declared.
					New_Prefix = case Prefix of
						undefined ->
							% Doesn't provide a
							% prefix, it must
							% be generated.
							% FIXME Generate a
							% random prefix.
							ok;
						_ ->
							Prefix
					end,
					NS_Decl = {"xmlns:" ++ New_Prefix,
					    atom_to_list(NS)},
					{
					    ?PREFIXED_NAME(New_Prefix,
						Name_S),
					    Converted_Attrs ++ [NS_Decl],
					    [{NS, New_Prefix} | Prefixed_NS]
					}
			end
	end,
	xmlnsattributes_to_xmlattributes2(Rest, New_Prefixed_NS,
	    [{New_Name, Value} | New_Converted_Attrs]);
xmlnsattributes_to_xmlattributes2([], Prefixed_NS, Converted_Attrs) ->
	{lists:reverse(Converted_Attrs), Prefixed_NS}.

%% @spec (XML_Element) -> XML_NS_Element
%%     XML_Element = xmlelement() | xmlcdata()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()} tuple.
%%
%% Other tuples are ignored.

xmlelement_to_xmlnselement(XML_Element) ->
	xmlelement_to_xmlnselement(XML_Element, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_NS_Element
%%     XML_Element = xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Prefix = string()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()} tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlnselement_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.

xmlelement_to_xmlnselement(XML_El, Default_NS, Prefixed_NS) ->
	{New_XML_El, _, _} = xmlelement_to_xmlnselement_and_ns_tables(XML_El,
	    Default_NS, Prefixed_NS),
	New_XML_El.

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> {XML_NS_Element, New_Default_NS, New_Prefixed_NS}
%%     XML_Element = xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Prefix = string()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     New_Default_NS = [NS]
%%     New_Prefixed_NS = [{NS, Prefix}]
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()} tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlnselement_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.
%%
%% This function will returned updated namespaces tables `New_Default_NS' and
%% `New_Prefixed_NS' which can be used for future calls.

xmlelement_to_xmlnselement_and_ns_tables(
    #xmlelement{name = Name, attrs = Attrs, children = Children},
    Default_NS, Prefixed_NS) ->
	% Udpate NS tables by looking at each attributes for NS declarations.
	% These later are removed at the same time.
	{Attrs2, Default_NS2, Prefixed_NS2} = update_ns_from_xmlattributes(
	    Attrs, Default_NS, Prefixed_NS),
	% Convert attributes and children to the new format.
	Attrs3 = xmlattributes_to_xmlnsattributes(Attrs2, Prefixed_NS2),
	Children2 = xmlelements_to_xmlnselements(Children,
	    Default_NS2, Prefixed_NS2),
	% Check the element namespace and convert it to the new format.
	XML_NS_Element = case string:tokens(Name, ":") of
		[Prefix, Real_Name] ->
			Real_Name_A = list_to_atom(Real_Name),
			case lists:keysearch(Prefix, 2,
			    Prefixed_NS2 ++ ?IMPLICIT_NAMESPACES) of
				{value, {NS, _Prefix}} ->
					#xmlnselement{
						ns = NS,
						prefix = Prefix,
						name = Real_Name_A,
						attrs = Attrs3,
						children = Children2
					};
				false ->
					% Namespace never declared.
					#xmlnselement{
						ns = undefined,
						prefix = Prefix,
						name = Real_Name_A,
						attrs = Attrs3,
						children = Children2
					}
			end;
		[Real_Name] ->
			Real_Name_A = list_to_atom(Real_Name),
			case Default_NS2 of
				[NS | _] ->
					#xmlnselement{
						ns = NS,
						prefix = undefined,
						name = Real_Name_A,
						attrs = Attrs3,
						children = Children2
					};
				_ ->
					% No default namespace declared.
					#xmlnselement{
						ns = undefined,
						prefix = undefined,
						name = Real_Name_A,
						attrs = Attrs3,
						children = Children2
					}
			end
	end,
	{XML_NS_Element, Default_NS2, Prefixed_NS2};
xmlelement_to_xmlnselement_and_ns_tables(
    #xmlendelement{name = Name}, Default_NS, Prefixed_NS) ->
	XML_NS_Element = case string:tokens(Name, ":") of
		[Prefix, Real_Name] ->
			Real_Name_A = list_to_atom(Real_Name),
			case lists:keysearch(Prefix, 2,
			    Prefixed_NS ++ ?IMPLICIT_NAMESPACES) of
				{value, {NS, _Prefix}} ->
					#xmlnsendelement{
						ns = NS,
						prefix = Prefix,
						name = Real_Name_A
					};
				false ->
					% Namespace never declared.
					#xmlnsendelement{
						ns = undefined,
						prefix = Prefix,
						name = Real_Name_A
					}
			end;
		[Real_Name] ->
			Real_Name_A = list_to_atom(Real_Name),
			case Default_NS of
				[NS | _] ->
					#xmlnsendelement{
						ns = NS,
						prefix = undefined,
						name = Real_Name_A
					};
				_ ->
					% No default namespace declared.
					#xmlnsendelement{
						ns = undefined,
						prefix = undefined,
						name = Real_Name_A
					}
			end
	end,
	{XML_NS_Element, Default_NS, Prefixed_NS};
xmlelement_to_xmlnselement_and_ns_tables(XML_El, Default_NS, Prefixed_NS) ->
	% xmlnslement() ot xmlcdata().
	{XML_El, Default_NS, Prefixed_NS}.

xmlelements_to_xmlnselements(undefined, _Default_NS, _Prefixed_NS) ->
	undefined;
xmlelements_to_xmlnselements([], _Default_NS, _Prefixed_NS) ->
	[];
xmlelements_to_xmlnselements(XML_Elements, Default_NS, Prefixed_NS) ->
	xmlelements_to_xmlnselements2(XML_Elements, [],
	    Default_NS, Prefixed_NS).

xmlelements_to_xmlnselements2([XML_Element | Rest], XML_NS_Elements,
    Default_NS, Prefixed_NS) ->
	XML_NS_Element = xmlelement_to_xmlnselement(XML_Element,
	    Default_NS, Prefixed_NS),
	xmlelements_to_xmlnselements2(Rest,
	    [XML_NS_Element | XML_NS_Elements], Default_NS, Prefixed_NS);
xmlelements_to_xmlnselements2([], XML_NS_Elements, _Default_NS, _Prefixed_NS) ->
	lists:reverse(XML_NS_Elements).

update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS) ->
	update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS, []).

update_ns_from_xmlattributes([{Name, Value} = Attr | Rest],
    Default_NS, Prefixed_NS, Purged_Attrs) ->
	case string:tokens(Name, ":") of
		["xmlns"] ->
			% Default NS declaration.
			update_ns_from_xmlattributes(Rest,
			    [list_to_atom(Value) | Default_NS],
			    Prefixed_NS,
			    Purged_Attrs);
		["xmlns", Prefix] ->
			% Prefixed NS declaration.
			update_ns_from_xmlattributes(Rest,
			    Default_NS,
			    [{list_to_atom(Value), Prefix} | Prefixed_NS],
			    Purged_Attrs);
		_ ->
			% Irrelevant attribute.
			update_ns_from_xmlattributes(Rest,
			    Default_NS, Prefixed_NS, Purged_Attrs ++ [Attr])
	end;
update_ns_from_xmlattributes([], Default_NS, Prefixed_NS, Purged_Attrs) ->
	{Purged_Attrs, Default_NS, Prefixed_NS}.

xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS) ->
	xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS, []).

xmlattributes_to_xmlnsattributes([{Name, Value} | Rest],
    Prefixed_NS, Converted_Attrs) ->
	New_Attr = case string:tokens(Name, ":") of
		[Prefix, Real_Name] ->
			Real_Name_A = list_to_atom(Real_Name),
			case lists:keysearch(Prefix, 2,
			    Prefixed_NS ++ ?IMPLICIT_NAMESPACES) of
				{value, {NS, _Prefix}} ->
					#xmlattr{
						ns = NS,
						prefix = Prefix,
						name = Real_Name_A,
						value = Value
					};
				false ->
					% Namespace never declared.
					#xmlattr{
						ns = undefined,
						prefix = Prefix,
						name = Real_Name_A,
						value = Value
					}
			end;
		[Real_Name] ->
			% Not attached to any namespace.
			Real_Name_A = list_to_atom(Real_Name),
			#xmlattr{
				ns = undefined,
				prefix = undefined,
				name = Real_Name_A,
				value = Value
			}
	end,
	xmlattributes_to_xmlnsattributes(Rest, Prefixed_NS,
	    Converted_Attrs ++ [New_Attr]);
xmlattributes_to_xmlnsattributes([], _Prefixed_NS, Converted_Attrs) ->
	Converted_Attrs.

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlnselement() | xmlelement()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Prefix = string()
%%     XML_Text = string()
%% @doc Serialize an XML document fragment to text.
%%
%% `Default_NS' and `Prefixed_NS' contain namespace declaration which
%% occured above this fragment in the tree. The order in the fist list
%% is important : declarations are sorted from the most recent one to
%% the oldest one.

document_fragment_to_list(El, Default_NS, Prefixed_NS) ->
	case El of
		#xmlnselement{} ->
			document_to_list(
			    xmlnselement_to_xmlelement(El,
			    Default_NS, Prefixed_NS));
		#xmlnsendelement{} ->
			document_to_list(
			    xmlnselement_to_xmlelement(El,
			    Default_NS, Prefixed_NS));
		#xmlelement{name = Name, attrs = Attrs, children = Els} ->
			Name_S = if
				is_atom(Name) -> atom_to_list(Name);
				true          -> Name
			end,
			case Els of
				undefined ->
					% Children may come later, we don't
					% close the tag.
					[$<, Name_S, attrs_to_list(Attrs), $>];
				[] ->
					[$<, Name_S, attrs_to_list(Attrs),
					    $/, $>];
				_ ->
					% NS stacks are passed to
					% document_fragment_to_list/3
					% again, but this isn't relevant
					% without namespace support.
					[$<, Name_S, attrs_to_list(Attrs), $>,
					    [document_fragment_to_list(E,
					        Default_NS, Prefixed_NS) ||
					        E <- Els],
					    $<, $/, Name_S, $>]
			end;
		#xmlendelement{name = Name} ->
			Name_S = if
				is_atom(Name) -> atom_to_list(Name);
				true          -> Name
			end,
			[$<, $/, Name_S, $>];
		#xmlpi{target = Target, value = Value} ->
			Target_S = if
				is_atom(Target) -> atom_to_list(Target);
				true            -> Target
			end,
			[$<, $?, Target_S, $\s, Value, $?, $>];
		#xmlcdata{cdata = CData} when is_binary(CData) ->
			% We avoid calling crypt/1 directly with the binary()
			% because it'll convert it back to binary().
			encode_entities(binary_to_list(CData));
		#xmlcdata{cdata = CData} ->
			encode_entities(CData)
	end.

attrs_to_list(Attrs) ->
	[attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
	[$\s, encode_entities(Name), $=, $", encode_entities(Value), $"].

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlnselement() | xmlelement()
%%     XML_Text = string()
%% @doc Serialize an XML document to text.

document_to_list(El) ->
	document_fragment_to_list(El, [], []).

%% @spec (XML_Elements) -> Cleaned_XML_Elements
%%     XML_Elements = [xmlnselement() | xmlelement() | xmlcdata() |
%%         xmlnsendelement() | xmlendelement()]
%%     Cleaned_XML_Elements = [xmlnselement() | xmlelement() | xmlcdata()]
%% @doc Remove any {@link xmlnsendelement()} or {@link xmlendelement()}
%% from the list of XML elements.
%%
%% This is primarily designed to work on returned value of {@link
%% parse/2} and {@link parse_final/2} when the `no_endelement' parser
%% option (see {@link xmlparseroption()}) wasn't specified at {@link
%% start_parser/1} time.

clear_endelement_tuples(XML_Elements) ->
	clear_endelement_tuples2(XML_Elements, []).

clear_endelement_tuples2([#xmlnsendelement{} | Rest], Result) ->
	clear_endelement_tuples2(Rest, Result);
clear_endelement_tuples2([#xmlendelement{} | Rest], Result) ->
	clear_endelement_tuples2(Rest, Result);
clear_endelement_tuples2([XML_Element | Rest], Result) ->
	clear_endelement_tuples2(Rest, [XML_Element | Result]);
clear_endelement_tuples2([], Result) ->
	lists:reverse(Result).

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @doc Replace sensible characters with entities.
%%
%% Processed characters are <tt>&amp;</tt>, <tt>&lt;</tt>,
%% <tt>&gt;</tt>, <tt>&quot;</tt>, <tt>&apos;</tt>.

encode_entities(S) when is_list(S) ->
	lists:flatten([case C of
		$& -> "&amp;";
		$< -> "&lt;";
		$> -> "&gt;";
		$" -> "&quot;";
		$' -> "&apos;";
		_ -> C
		end || C <- S]);

encode_entities(S) when is_binary(S) ->
	encode_entities2(S, []).

encode_entities2(<<C:8, Rest/binary>>, New_S) ->
	New_C = case C of
		$& -> <<"&amp;">>;
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$" -> <<"&quot;">>;
		$' -> <<"&apos;">>;
		_  -> C
	end,
	encode_entities2(Rest, [New_C | New_S]);
encode_entities2(<<>>, New_S) ->
	list_to_binary(lists:reverse(New_S)).

% --------------------------------------------------------------------
% Utilities.
% --------------------------------------------------------------------

load_driver() ->
	case exmpp_internals:load_driver(?DRIVER_NAME) of
		{error, Reason} ->
			{error, Reason};
		ok ->
			case exmpp_internals:open_port(?DRIVER_NAME) of
				{error, Reason} ->
					exmpp_internals:unload_driver(
					    ?DRIVER_NAME),
					{error, Reason};
				Port ->
					Port
			end
	end.

unload_driver() ->
	exmpp_internals:unload_driver(?DRIVER_NAME).

handle_options([namespace | Rest], #xml_parser{port = P} = Parser) ->
	Ret = port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(true)),
	case binary_to_term(Ret) of
		ok ->
			handle_options(Rest, Parser);
		Error ->
			Error
	end;
handle_options([no_namespace | Rest], #xml_parser{port = P} = Parser) ->
	Ret = port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(false)),
	case binary_to_term(Ret) of
		ok ->
			handle_options(Rest, Parser);
		Error ->
			Error
	end;

handle_options([name_as_atom | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NAMEASATOM, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([name_as_string | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NAMEASATOM, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([ns_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_NS, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([no_ns_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_NS, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([names_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_NAMES, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([no_names_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_NAMES, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([attrs_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_ATTRS, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([no_attrs_check | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_CHECK_ATTRS, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([no_maxsize | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{maxsize, infinity} | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{maxsize, Max} | Rest], #xml_parser{port = P} = Parser)
    when is_integer(Max), Max >= 0 ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(Max)),
	handle_options(Rest, Parser);

handle_options([no_root_depth | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ROOTDEPTH, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{root_depth, none} | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ROOTDEPTH, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{root_depth, Depth} | Rest], #xml_parser{port = P} = Parser)
    when is_integer(Depth), Depth >= 0 ->
	port_control(P, ?EXPAT_SET_ROOTDEPTH, term_to_binary(Depth)),
	handle_options(Rest, Parser);

handle_options([endelement | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ENDELEMENT, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([no_endelement | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ENDELEMENT, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([Unknown_Option | _Rest], _Parser) ->
	{error, {unknown_option, Unknown_Option}};

handle_options([], Parser) ->
	Parser.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type xmlparser().
%% Handler for the Expat parser, initialized with a call to {@link
%% start_parser/0}.

%% @type xmlparseroption() = Namespace_Option | Names_Format | Checks | Stanza_Max_Size | Root_Depth | Send_End_Element
%%     Namespace_Option = namespace | no_namespace
%%     Name_Format = name_as_atom | name_as_string
%%     Checks = NS_Check | Names_Check | Attrs_Check
%%       NS_Check = ns_check | no_ns_check
%%       Names_Check = names_check | no_names_check
%%       Attrs_Check = attrs_check | no_attrs_check
%%     Stanza_Max_Size  = no_maxsize | {maxsize, infinity} | {maxsize, Size}
%%     Root_Depth = no_root_depth | {root_depth, none} | {root_depth, Depth}
%%     Send_End_Element = endelement | noendelement.
%% The `namespace' and `no_namespace' flags enable or disable the
%% support for namespaces respectively. Note that the support is very
%% experimental. Tag and attribute namespaces are supported.
%%
%% <br/><br/>
%% The `name_as_atom' and `name_as_string' options set if element and
%% attribute names should be encoded as an {@link atom()} or a {@link
%% string()} respectively. "Should" because if names or attributes
%% checks fail, a name will be encoded as a `string()' (see next
%% option).
%%
%% <br/><br/>
%% The `Checks' flags enable or disable the control of a namespace, an
%% element name or an attribute name if `name_as_atom' is enabled. This
%% is to avoid atom() table pollution and overflow. If a check says
%% that the verified string is known, it'll be encoded as an atom() in
%% the tuple; otherwise it'll be encoded as a string(). It's highly
%% recommended to keep these checks enabled.
%%
%% <br/><br/>
%% The `maxsize' option limits the size in bytes of a stanza to avoid
%% deny of service at the parser level. Actually, this limit is only
%% verified against the length of the data provided and the counter is
%% reset to zero when an element is found. The caveats is that if the
%% limits is, eg., 15 and the data is `<foo></foo><bar></bar>', the
%% parser will return an error because the whole chunk is 22 bytes,
%% despite each stanza contains 11 bytes.
%%
%% <br/><br/>
%% The `root_depth' option specicifies at which level the parser stop
%% to split each node and start to produce trees. For example, if the
%% root depth is 0, the parser will return a unique tree for the whole
%% document. If the root depth is 1, then `<stream>' will produce
%% an element without any children and `<presence>' will produce a
%% tree with all its children. With `no_root_depth' (or `{root_depth,
%% none}'), no tree will be made, ie, each opening tag will produce an
%% element without any children.
%%
%% <br/><br/>
%% The `endelement' and `no_endelement' select if the parser must
%% produce {@link xmlendelement()} or {@link xmlnsendelement()} when it
%% encouters a closing tag above `root_depth'.

%% @type xmlelement() = {xmlelement, Name, Attrs, Children}
%%     Name = string() | atom()
%%     Attrs = [xmlattribute()]
%%     Children = undefined | [xmlelement() | xmlcdata()].
%% Record representing an XML tag.

%% @type xmlnselement() = {xmlnselement, NS, Name, Attrs, Children}
%%     NS = atom()
%%     Name = string() | atom()
%%     Attrs = [xmlnsattribute()]
%%     Children = undefined | [xmlnselement() | xmlcdata()].
%% Record representing an XML tag when namespace support is enabled.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlattribute() = {Name, Value}
%%     Name = string() | atom()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlnsattribute() = {xmlattr, NS, Name, Value}
%%     NS = atom()
%%     Name = string() | atom()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlendelement() = {xmlendelement, Name}
%%     Name = string().
%% Record representing an XML closing tag for nodes above the configured
%% `root_depth' (see {@link xmlparseroption()}).

%% @type xmlnsendelement() = {xmlnsendelement, NS, Name}
%%     NS = atom()
%%     Name = string() | atom().
%% Record representing an XML closing tag when namespace support is
%% enabled, for nodes above the configured `root_depth' (see {@link
%% xmlparseroption()}).

%% @type pathcomponent() = {element, Elem_Name} | {element, NS, Elem_Name} | {attribute, Attr_Name} | {attribute, NS, Attr_Name} | cdata
%%     NS = atom()
%%     Elem_Name = string() | atom()
%%     Attr_Name = string() | atom().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.
