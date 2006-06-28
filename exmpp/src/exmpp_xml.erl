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
%% Note that <strong>namespace support is experimental</strong> at
%% this time and thus isn't recommended for production.
%% </p>
%%
%% <p><strong>The API hasn't been validated yet</strong>.
%% What's left to be done:</p>
%% <ul>
%% <li>finish namespace support for attributes (for now, only `xml:'
%% prefix is supported and not in the best way)</li>
%% <li>add validation for namespace</li>
%% <li>add validation for element and attribute names</li>
%% </ul>

-module(exmpp_xml).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
	start_parser/0,
	start_parser/1,
	stop_parser/1,
	reload_driver/0,
	parse/2,
	parse_final/2
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
	set_attribute_from_list/3,
	set_attribute_from_list/4,
	set_attribute/3,
	set_attribute/4,
	remove_attribute_from_list/2,
	remove_attribute_from_list/3,
	remove_attribute/2,
	remove_attribute/3
]).
-export([
	get_element_by_name/2,
	get_element_by_name/3,
	append_child/2
]).
-export([
	get_cdata_from_list/1,
	get_cdata/1,
	remove_cdata_from_list/1,
	remove_cdata/1
]).
-export([
	get_path/2,
	xmlnselement_to_xmlelement/1,
	xmlnselement_to_xmlelement/3,
	document_fragment_to_list/3,
	document_to_list/1,
	clear_endelement_tuples/1,
	encode_entities/1
]).

-ifdef('WITH_DEPRECATED_API').
% Deprecated API
-export([
	get_subtag/2,
	add_child/2,
	get_tag_attr/2,
	get_tag_attr_s/2,
	get_attr/2,
	get_attr_s/2,
	replace_attr/3,
	replace_tag_attr/3,
	remove_attr/2,
	remove_tag_attr/2,
	get_tag_cdata/1,
	element_to_string/1,
	crypt/1
]).
-endif. % -ifdef('WITH_DEPRECATED_API').

-record(xml_parser, {
	port
}).

-define(DRIVER_NAME, expat_drv).
-define(DRIVER_NAME_S, "expat_drv").

-define(EXPAT_SET_NSPARSER,   1).
-define(EXPAT_SET_NAMEASATOM, 2).
-define(EXPAT_SET_MAXSIZE,    3).
-define(EXPAT_SET_ROOTDEPTH,  4).
-define(EXPAT_SET_ENDELEMENT, 5).
-define(EXPAT_PARSE,          6).
-define(EXPAT_PARSE_FINAL,    7).

-define(DEFAULT_PARSER_OPTIONS, [
	% no_namespace, % Handled by start_parser/1
	name_as_string,
	no_endelement,
	{root_depth, 0},
	no_maxsize
]).

-define(PREFIXED_NAME(P, N), P ++ ":" ++ N).

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
			New_Parser = handle_options(
			    ?DEFAULT_PARSER_OPTIONS ++ Options2, Parser),
			{ok, New_Parser}
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
	port_close(Port).

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

%% @spec () -> ok | {error, Reason}
%% @doc Reload the driver linked-in library.
%%
%% This should be done after upgrading the application if the C source
%% code has been modified. This MUST NOT be called is there are parsers
%% in use.
%%
%% @see erl_ddll:load_driver/2.

reload_driver() ->
	erl_ddll:unload_driver(?DRIVER_NAME),
	Dirs = driver_dirs(),
	load_driver1(Dirs, undefined).

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

set_attribute_from_list(Attrs, Name, Value) ->
	set_attribute_from_list2(Attrs, Name, Value, []).

set_attribute_from_list2([Attr | Rest], Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		{Name, _Value} ->
			New_Attr = {Name, Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_from_list2(Rest, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_from_list2([], Name, Value, New_Attrs) ->
	New_Attr = case New_Attrs of
		[#xmlattr{} | _] ->
			#xmlattr{name = Name, value = Value};
		[{_, _} | _] ->
			{Name, Value};
		_ ->
			#xmlattr{name = Name, value = Value}
	end,
	New_Attrs ++ New_Attr.

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

set_attribute_from_list(Attrs, NS, Name, Value) ->
	set_attribute_from_list2(Attrs, NS, Name, Value, []).

set_attribute_from_list2([Attr | Rest], NS, Name, Value, New_Attrs) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			New_Attr = Attr#xmlattr{value = Value},
			New_Attrs ++ [New_Attr] ++ Rest;
		_ ->
			set_attribute_from_list2(Rest, NS, Name, Value,
			    New_Attrs ++ [Attr])
	end;
set_attribute_from_list2([], NS, Name, Value, New_Attrs) ->
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
%%     XML_Element = [xmlnselement() | xmlelement()]
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

%% @spec (Attrs, Attr_Name) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Remove attribute named `Attr_Name' and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute_from_list([Attr | Rest] = Attrs, Name) ->
	case Attr of
		#xmlattr{name = Name} ->
			Rest;
		{Name, _Value} ->
			Rest;
		_ ->
			remove_attribute_from_list(Attrs, Name)
	end;
remove_attribute_from_list([] = Attrs, _Name) ->
	Attrs.

%% @spec (Attrs, NS, Attr_Name) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = string() | atom()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute_from_list([Attr | Rest] = Attrs, NS, Name) ->
	case Attr of
		#xmlattr{ns = NS, name = Name} ->
			Rest;
		_ ->
			remove_attribute_from_list(Attrs, Name)
	end;
remove_attribute_from_list([] = Attrs, _NS, _Name) ->
	Attrs.

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
	false.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Child = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Append `Child' to `XML_Element''s children list.

append_child(#xmlnselement{children = Children} = XML_Element, Child) ->
	New_Children = lists:append(Children, [Child]),
	XML_Element#xmlnselement{children = New_Children};

append_child(#xmlelement{children = Children} = XML_Element, Child) ->
	New_Children = lists:append(Children, [Child]),
	XML_Element#xmlelement{children = New_Children}.

%% @spec ([Children]) -> CData
%%     Children = xmlnselement() | xmlelement()
%%     CData = string()
%% @doc Concatenate and return any character data from th given children list.

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
	get_cdata_from_list(Children).

%% @spec (Children) -> New_Children
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_Children = [xmlnselement() | xmlelement()]
%% @doc Remove any character data from the given XML element children list.

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
get_path(XML_Element, [{attribute, Name}]) ->
	get_attribute(Name, XML_Element);
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

%% @spec (XML_NS_Element, NS_Stack, Prefix_Stack) -> XML_Element
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlnselement()} to an {@link xmlelement()} tuple.
%%
%% Other tuples are ignored.
%%
%% `NS_Stack' and `Prefix_Stack' contain namespace declaration which
%% occured above this fragment in the tree. The order in these lists is
%% important : declarations are sorted from the most recent one to the
%% oldest one.
%%
%% This may be useful in XMPP context where a majority of clients or
%% servers expects a `stream' prefix for the `<stream>' tag and the
%% default namespace declaration in this same element.

xmlnselement_to_xmlelement(
    #xmlnselement{ns = NS, prefix = Prefix, name = Name, attrs = Attrs,
    children = Children}, NS_Stack, Prefix_Stack) ->
	case NS_Stack of
		[NS | _] ->
			% Same namespace as parent.
			New_NS_Stack = NS_Stack,
			New_Prefix_Stack = Prefix_Stack,
			New_Name = case Prefix_Stack of
				[undefined | _] ->
					Name;
				[P | _] ->
					?PREFIXED_NAME(P, Name)
			end,
			New_Attrs = xmlnsattributes_to_xmlattributes(
			    Attrs, New_NS_Stack, New_Prefix_Stack);
		_ ->
			% Different namespace.
			New_NS_Stack = [NS | NS_Stack],
			New_Prefix_Stack = [Prefix | Prefix_Stack],
			Attrs2 = xmlnsattributes_to_xmlattributes(
			    Attrs, New_NS_Stack, New_Prefix_Stack),
			case NS of
				undefined ->
					New_Name = Name,
					New_Attrs = Attrs2;
				_ ->
					case Prefix of
						undefined ->
							New_Name = Name,
							New_Attrs =
							    [{"xmlns",
							    atom_to_list(NS)} |
							    Attrs2];
						_ ->
							New_Name  =
							    ?PREFIXED_NAME(
							    Prefix, Name),
							New_Attrs =
							    [{"xmlns:" ++
							    Prefix,
							    atom_to_list(NS)} |
							    Attrs2]
					end
			end
	end,
	New_Children = xmlnselements_to_xmlelements(Children,
	    New_NS_Stack, New_Prefix_Stack),
	#xmlelement{name = New_Name, attrs = New_Attrs,
	    children = New_Children};
xmlnselement_to_xmlelement(XML_El, _NS_Stack, _Prefix_Stack) ->
	% xmlelement() or xmlcdata().
	XML_El.

xmlnselements_to_xmlelements([], _NS_Stack, _Prefix_Stack) ->
	[];
xmlnselements_to_xmlelements(XML_Elements, NS_Stack, Prefix_Stack) ->
	xmlnselements_to_xmlelements2(XML_Elements, [],
	    NS_Stack, Prefix_Stack).

xmlnselements_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
    NS_Stack, Prefix_Stack) ->
	New_XML_NS_Element = xmlnselement_to_xmlelement(XML_NS_Element,
	    NS_Stack, Prefix_Stack),
	xmlnselements_to_xmlelements2(Rest,
	    [New_XML_NS_Element | XML_Elements], NS_Stack, Prefix_Stack);
xmlnselements_to_xmlelements2([], XML_Elements, _NS_Stack, _Prefix_Stack) ->
	lists:reverse(XML_Elements).

xmlnsattributes_to_xmlattributes(Attrs, _NS_Stack, _NS_Prefix) ->
	xmlnsattributes_to_xmlattributes2(Attrs, []).

xmlnsattributes_to_xmlattributes2([#xmlattr{} = Attr | Rest], New_Attrs) ->
	% For now, this is pretty basic and only handles `xml:' prefix.
	New_Name = case Attr#xmlattr.ns of
		'http://www.w3.org/XML/1998/namespace' ->
			?PREFIXED_NAME("xml", Attr#xmlattr.name);
		_ ->
			Attr#xmlattr.name
	end,
	xmlnsattributes_to_xmlattributes2(Rest,
	    [{New_Name, Attr#xmlattr.value} | New_Attrs]);
xmlnsattributes_to_xmlattributes2([], New_Attrs) ->
	lists:reverse(New_Attrs).

%% @spec (XML_Element, NS_Stack, Prefix_Stack) -> XML_Text
%%     XML_Element = xmlnselement() | xmlelement()
%%     NS_Stack = [atom()]
%%     Prefix_Stack = [string()]
%%     XML_Text = string()
%% @doc Serialize an XML document fragment to text.
%%
%% `NS_Stack' and `Prefix_Stack' contain namespace declaration which
%% occured above this fragment in the tree. The order in these lists is
%% important : declarations are sorted from the most recent one to the
%% oldest one.

document_fragment_to_list(El, NS_Stack, Prefix_Stack) ->
	case El of
		#xmlnselement{} ->
			document_to_list(
			    xmlnselement_to_xmlelement(El,
			    NS_Stack, Prefix_Stack));
		#xmlelement{name = Name, attrs = Attrs, children = Els} ->
			if
				Els /= [] ->
					Name2 = if
						is_atom(Name) ->
							atom_to_list(Name);
						true ->
							Name
					end,
					% Stacks are passed to
					% document_fragment_to_list/3
					% again, but this isn't relevant
					% without namespace support.
					[$<, Name2, attrs_to_list(Attrs), $>,
					    [document_fragment_to_list(E,
					        NS_Stack, Prefix_Stack) ||
					        E <- Els],
					    $<, $/, Name2, $>];
				true ->
					[$<, Name, attrs_to_list(Attrs),
					    $/, $>]
			end;
		{xmlcdata, CData} when is_binary(CData) ->
			% We avoid calling crypt/1 directly with the binary()
			% because it'll convert it back to binary().
			encode_entities(binary_to_list(CData));
		{xmlcdata, CData} ->
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
	[case C of
		$& -> "&amp;";
		$< -> "&lt;";
		$> -> "&gt;";
		$" -> "&quot;";
		$' -> "&apos;";
		_ -> C
		end || C <- S];

encode_entities(S) when is_binary(S) ->
	list_to_binary(encode_entities(binary_to_list(S))).

% --------------------------------------------------------------------
% Deprecated API (almost renamed functions).
% --------------------------------------------------------------------

-ifdef('WITH_DEPRECATED_API').

%% @spec (XML_Element, Name) -> XML_Subelement | false
%%     XML_Element = xmlnselement() | xmlelement()
%%     Name = string() | atom()
%%     XML_Subelement = xmlelement() | xmlnselement()
%% @deprecated Please use {@link get_element_by_name/2}.
%% @doc Search in the children of `XML_Element' an element named `Name'.

get_subtag(XML_Element, Name) ->
	get_element_by_name(XML_Element, Name).

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Child = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @deprecated Please use {@link append_child/2}.
%% @doc Add `Child' to `XML_Element''s children.
%%
%% The new `Child' will be at the beginning of the list.

add_child(#xmlelement{children = Children} = XML_Element, Child) ->
	New_Children = [Child | Children],
	XML_Element#xmlelement{children = New_Children};

add_child(#xmlnselement{children = Children} = XML_Element, Child) ->
	New_Children = [Child | Children],
	XML_Element#xmlnselement{children = New_Children}.

%% @spec (Attr_Name, XML_Element) -> {value, Value} | false
%%     Attr_Name = string() | atom()
%%     XML_Element = xmlnselement() | xmlelement()
%% @deprecated Please use {@link get_attribute/2}.
%% @doc Return the `Attr_Name' attribute value (in a tuple) from the
%% `XML_Element' element.

get_tag_attr(Attr_Name, #xmlelement{attrs = Attrs}) ->
	get_attr(Attr_Name, Attrs);

get_tag_attr(Attr_Name, #xmlnselement{attrs = Attrs}) ->
	get_attr(Attr_Name, Attrs).

%% @spec (Attr_Name, XML_Element) -> Value | false
%%     Attr_Name = string() | atom()
%%     XML_Element = xmlnselement() | xmllement()
%% @deprecated Please use {@link get_attribute/2}.
%% @doc Return the `Attr_Name' attribute value from the `XML_Element'
%% element.

get_tag_attr_s(Attr_Name, #xmlelement{attrs = Attrs}) ->
	get_attr_s(Attr_Name, Attrs);

get_tag_attr_s(Attr_Name, #xmlnselement{attrs = Attrs}) ->
	get_attr_s(Attr_Name, Attrs).

%% @spec (Attr_Name, Attrs) -> {value, Value} | false
%%     Attr_Name = string() | atom()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%% @deprecated Please use {@link get_attribute_from_list/2}.
%% @doc Return the `Attr_Name' attribute value (in a tuple) from the
%% xmlattribute() list.

get_attr(Attr_Name, Attrs) ->
	case get_attribute_node_from_list(Attrs, Attr_Name) of
		#xmlattr{value = Value} ->
			{value, Value};
		{_Name, Value} ->
			{value, Value};
		false ->
			false
	end.

%% @spec (Attr_Name, Attrs) -> Value | false
%%     Attr_Name = string() | atom()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%% @deprecated Please use {@link get_attribute_from_list/2}.
%% @doc Return the `Attr_Name' attribute value from the xmlattribute()
%% list.

get_attr_s(Attr_Name, Attrs) ->
	case get_attr(Attr_Name, Attrs) of
		{value, Value} ->
			Value;
		false ->
			""
	end.

%% @spec (Attr_Name, Attr_Value, Attrs) -> New_Attrs
%%     Attr_Name = string() | atom()
%%     Attr_Value = string()
%%     Attrs = [xmlattribute()]
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @deprecated Please use {@link set_attribute_from_list/3}.
%% @doc Replace `Attr_Name' value with `Attr_Value'.

replace_attr(Attr_Name, Attr_Value, Attrs) ->
	set_attribute_from_list(Attrs, Attr_Name, Attr_Value).

%% @spec (Attr_Name, Value, XML_Element) -> New_XML_Element
%%     Attr_Name = string() | atom()
%%     Value = string()
%%     XML_Element = xmlelement() | xmlnselement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @deprecated Please use {@link set_attribute/3}.
%% @doc Replace the value of `Attr_Name' attribute with `Value' in the
%% given `XML_Element' element.

replace_tag_attr(Attr_Name, Attr_Value, XML_Element) ->
	set_attribute(XML_Element, Attr_Name, Attr_Value).

%% @spec (Attr_Name, Attrs) -> New_Attrs
%%     Attr_Name = string() | atom()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @deprecated Please use {@link remove_attribute_from_list/2}.
%% @doc Remove an attribute and return the new list.

remove_attr(Attr_Name, Attrs) ->
	remove_attribute_from_list(Attrs, Attr_Name).

%% @spec (Attr_Name, XML_Element) -> New_XML_Element
%%     Attr_Name = string() | atom()
%%     XML_Element = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @deprecated Please use {@link remove_attribute_from_list/2}.
%% @doc Remove an attribute and return the new element.

remove_tag_attr(Attr_Name, XML_Element) ->
	remove_attribute(XML_Element, Attr_Name).

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlnselement() | xmlelement()
%%     CData = string()
%% @deprecated Please use {@link get_cdata/1}.
%% @doc Concatenate and return any character data of the given XML element.

get_tag_cdata(XML_Element) ->
	get_cdata(XML_Element).

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlnselement() | xmlelement()
%%     Path = [pathcomponent()]
%%     XML_Subelement = xmlnselement() | xmlelement()
%%     Attr_Value = string()
%%     CData = string()
%%     Not_Found = nil()
%% @deprecated Please use {@link get_path/2}.
%% @doc Follow the given path and return what's pointed by the last
%% component of it.

get_path_s(XML_Element, Path) ->
	get_path_s(XML_Element, Path).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlnselement() | xmlelement()
%%     XML_Text = string()
%% @deprecated Please use {@link document_to_list/1}.
%% @doc Serialize an XML tree to play text.

element_to_string(XML_Element) ->
	document_to_list(XML_Element).

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @deprecated Please use {@link encode_entities/1}.
%% @doc Replace sensible characters with entities.

crypt(CData) ->
	encode_entities(CData).

-endif. % -ifdef('WITH_DEPRECATED_API').

% --------------------------------------------------------------------
% Utilities.
% --------------------------------------------------------------------

driver_dirs() ->
	case code:priv_dir(exmpp:app()) of
		{error, _Reason} -> ["priv/lib", "../priv/lib"];
		Priv_Dir         -> [Priv_Dir ++ "/lib"]
	end.

load_driver() ->
	erl_ddll:start(),
	{ok, Drivers} = erl_ddll:loaded_drivers(),
	case lists:member(?DRIVER_NAME_S, Drivers) of
		true ->
			% Driver already loaded, just open a new port.
			load_driver2();
		false ->
			% Load the driver, then open a port.
			Dirs = driver_dirs(),
			load_driver1(Dirs, undefined)
	end.

load_driver1([Dir | Rest], _Reason) ->
	case erl_ddll:load_driver(Dir, ?DRIVER_NAME) of
		ok ->
			load_driver2();
		{error, Reason} ->
			load_driver1(Rest, Reason)
	end;

load_driver1([], Reason) ->
	error_logger:info_msg([{error, Reason}]),
	{error, Reason}.

load_driver2() ->
	open_port({spawn, ?DRIVER_NAME_S}, []).

handle_options([namespace | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([no_namespace | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([name_as_atom | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NAMEASATOM, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([name_as_string | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NAMEASATOM, term_to_binary(false)),
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

handle_options([], Parser) ->
	Parser.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type xmlparser().
%% Handler for the Expat parser, initialized with a call to {@link
%% start_parser/0}.

%% @type xmlparseroption() = Namespace_Option | Stanza_Max_Size | Root_Depth | Send_End_Element
%%     Namespace_Option = namespace | no_namespace
%%     Stanza_Max_Size  = no_maxsize | {maxsize, infinity} | {maxsize, Size}
%%     Root_Depth = {root_depth, Depth}
%%     Send_End_Element = endelement | noendelement.
%% The `namespace' and `no_namespace' flags enable or disable the
%% support for namespaces respectively. Note that the support is very
%% experimental. Tag and attribute namespaces are supported.
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
%% document. If the root depth is 1, then `<stream>' will produce an
%% element without any children and `<presence' will produce a tree with
%% all its children.
%%
%% <br/><br/>
%% The `endelement' and `no_endelement' select if the parser must
%% produce {@link xmlendelement()} or {@link xmlnsendelement()} when it
%% encouters a closing tag above `root_depth'.

%% @type xmlelement() = {xmlelement, Name, Attrs, Children}
%%     Name = string() | atom()
%%     Attrs = [xmlattribute()]
%%     Children = [xmlelement() | xmlcdata()].
%% Record representing an XML tag.

%% @type xmlnselement() = {xmlnselement, NS, Name, Attrs, Children}
%%     NS = atom()
%%     Name = string() | atom()
%%     Attrs = [xmlnsattribute()]
%%     Children = [xmlnselement() | xmlcdata()].
%% Record representing an XML tag when namespace support is enabled.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlattribute() = {Name, Value}
%%     Name = string()
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
%%     Name = string().
%% Record representing an XML closing tag when namespace support is
%% enabled, for nodes above the configured `root_depth' (see {@link
%% xmlparseroption()}).

%% @type pathcomponent() = {element, Elem_Name} | {attribute, Attr_Name} | cdata
%%     Elem_Name = string()
%%     Attr_Name = string().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.
