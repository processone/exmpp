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

% Parser.
-export([
  start_parser/0,
  start_parser/1,
  stop_parser/1,
  add_known_nss/2,
  add_known_names/2,
  add_known_attrs/2,
  parse/2,
  parse_final/2,
  parse_document/1,
  parse_document/2,
  parse_document_fragment/1,
  parse_document_fragment/2,
  port_revision/1
]).

% Attribute handling.
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

% Element handling.
-export([
  get_element_by_name/2,
  get_element_by_name/3,
  get_element_by_ns/2,
  get_elements_by_name/3,
  get_elements_by_name/2,
  get_child_elements/1,
  prepend_child/2,
  prepend_children/2,
  append_child/2,
  append_children/2,
  replace_child/3,
  set_children/2
]).

% Character data handling.
-export([
  get_cdata_from_list/1,
  get_cdata_from_list_as_list/1,
  get_cdata/1,
  get_cdata_as_list/1,
  normalize_cdata_in_list/1,
  normalize_cdata/1,
  set_cdata_in_list/2,
  set_cdata/2,
  remove_cdata_from_list/1,
  remove_cdata/1
]).

% Misc. functions on the whole XML tree.
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
  escape_using_entities/1,
  escape_using_cdata/1,
  internal_escaping_function_name/0
]).

-record(xml_parser, {
  port
}).

-define(DRIVER_NAME, exmpp_expat).

-define(EXPAT_SET_NSPARSER,     1).
-define(EXPAT_SET_NAMEASATOM,   2).
-define(EXPAT_SET_CHECK_NS,     3).
-define(EXPAT_SET_CHECK_NAMES,  4).
-define(EXPAT_SET_CHECK_ATTRS,  5).
-define(EXPAT_SET_MAXSIZE,      6).
-define(EXPAT_SET_ROOTDEPTH,    7).
-define(EXPAT_SET_ENDELEMENT,   8).
-define(EXPAT_ADD_KNOWN_NS,     9).
-define(EXPAT_ADD_KNOWN_NAME,  10).
-define(EXPAT_ADD_KNOWN_ATTR,  11).
-define(EXPAT_PARSE,           12).
-define(EXPAT_PARSE_FINAL,     13).
-define(EXPAT_SVN_REVISION,    14).

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

-ifdef(ESCAPE_USING_CDATA_SECTIONS).
-define(ESCAPE(CData), escape_using_cdata(CData)).
-else.
-define(ESCAPE(CData), escape_using_entities(CData)).
-endif.

-define(IMPLICIT_PREFIXED_NS, [
  {?NS_XML, "xml"}
]).

% --------------------------------------------------------------------
% Parsing functions (interface to the Expat port driver).
% --------------------------------------------------------------------

%% @spec () -> Parser
%%     Parser = xmlparser()
%% @doc Initialize the Expat port driver with default options.
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

%% @spec (Options) -> Parser
%%     Options = [xmlparseroption()]
%%     Parser = xmlparser()
%% @throws {xml_parser, options, Reason, Infos}
%% @doc Initialize the Expat port driver with given `Options'.
%%
%% You must call this function before any use of functions {@link
%% parse/2} or {@link parse_final/2}. The returned `Parser' must be
%% given as the first argument for those functions. When finished, you
%% must free this parser with the {@link stop_parser/1}. Here is an
%% example:
%% ```
%% fun() ->
%%     Parser = xml:start_parser(),
%%     xml:parse(Parser, "<stream version='1.0'><presence/></stream>"),
%%     xml:stop_parser(Parser).
%% '''

start_parser(Options) ->
    try
        % First, load the driver and open a port.
        exmpp_internals:load_driver(?DRIVER_NAME),
        Port = exmpp_internals:open_port(?DRIVER_NAME),

        % `no_namespace' is the default.
        Options2 = case lists:member(no_namespace, Options) of
            true ->
                Options;
            false ->
                case lists:member(namespace, Options) of
                    true  -> Options;
                    false -> [no_namespace | Options]
                end
        end,

        % Check options.
        case handle_options(?DEFAULT_PARSER_OPTIONS ++ Options2,
          #xml_parser{port = Port}) of
            {error, Reason, Infos} ->
                exmpp_internals:close_port(Port),
                throw({xml_parser, options, Reason, Infos});
            Parser ->
                Parser
        end
    catch
        throw:{port_driver, load, _, _} = Exception ->
            throw(Exception);
        throw:Exception ->
            exmpp_internals:unload_driver(?DRIVER_NAME),
            throw(Exception)
    end.

%% @spec (Parser) -> ok
%%     Parser = xmlparser()
%% @doc Stop the Expat port driver.
%%
%% This must be called when you are done with the `Parser' returned by
%% {@link start_parser/0}.
%%
%% @see start_parser/0. `start_parser/0' for an example

stop_parser(#xml_parser{port = Port} = _Parser) ->
    unlink(Port),
    exmpp_internals:close_port(Port),
    exmpp_internals:unload_driver(?DRIVER_NAME),
    ok.

%% @spec (Parser, NS_List) -> ok
%%     Parser = xmlparser()
%%     NS_List = [NS]
%%     NS = atom() | string()
%% @doc Tell the parser that `NS' are known namespaces.
%%
%% If enabled, all occurences of these namespaces will be represented as
%% an atom().

add_known_nss(Parser, [NS | Rest]) ->
    add_known_ns(Parser, NS),
    add_known_nss(Parser, Rest);
add_known_nss(_Parser, []) ->
    ok.

add_known_ns(Parser, NS) when is_atom(NS) ->
    add_known_ns(Parser, atom_to_list(NS));
add_known_ns(#xml_parser{port = Port} = _Parser, NS) ->
    port_control(Port, ?EXPAT_ADD_KNOWN_NS, term_to_binary(NS)).

%% @spec (Parser, Names_List) -> ok
%%     Parser = xmlparser()
%%     Names_List = [Name]
%%     Name = atom() | string()
%% @doc Tell the parser that `Name' are known element names.
%%
%% If enabled, all occurences of these names will be represented as
%% an atom().

add_known_names(Parser, [Name | Rest]) ->
    add_known_name(Parser, Name),
    add_known_names(Parser, Rest);
add_known_names(_Parser, []) ->
    ok.

add_known_name(Parser, Name) when is_atom(Name) ->
    add_known_name(Parser, atom_to_list(Name));
add_known_name(#xml_parser{port = Port} = _Parser, Name) ->
    port_control(Port, ?EXPAT_ADD_KNOWN_NAME, term_to_binary(Name)).

%% @spec (Parser, Attrs_List) -> ok
%%     Parser = xmlparser()
%%     Attrs_List = [Attr]
%%     Attr = atom() | string()
%% @doc Tell the parser that `Attr' are known element attributes.
%%
%% If enabled, all occurences of these attributes will be represented as
%% an atom().

add_known_attrs(Parser, [Attr | Rest]) ->
    add_known_attr(Parser, Attr),
    add_known_attrs(Parser, Rest);
add_known_attrs(_Parser, []) ->
    ok.

add_known_attr(Parser, Attr) when is_atom(Attr) ->
    add_known_attr(Parser, atom_to_list(Attr));
add_known_attr(#xml_parser{port = Port} = _Parser, Attr) ->
    binary_to_term(port_control(Port, ?EXPAT_ADD_KNOWN_ATTR,
      term_to_binary(Attr))).

%% @spec (Parser, Data) -> [XML_Element] | continue
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlelement() | xmlnselement() | xmlendelement() | xmlnsendelement()
%% @throws {xml_parser, parsing, Reason, undefined} |
%%         {xml_parser, parsing, malformed_xml, Reason}
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
    case binary_to_term(port_control(Port, ?EXPAT_PARSE, Data)) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            throw({xml_parser, parsing, Reason, undefined});
        {xmlerror, Reason} ->
            throw({xml_parser, parsing, malformed_xml, Reason})
    end.

%% @spec (Parser, Data) -> [XML_Element] | done
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlelement() | xmlnselement() | xmlendelement() | xmlnsendelement()
%% @throws {xml_parser, parsing, Reason, undefined} |
%%         {xml_parser, parsing, malformed_xml, Reason}
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
    case binary_to_term(port_control(Port, ?EXPAT_PARSE_FINAL, Data)) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            throw({xml_parser, parsing, Reason, undefined});
        {xmlerror, Reason} ->
            throw({xml_parser, parsing, malformed_xml, Reason})
    end.

%% @spec (Document) -> [XML_Element] | done
%%     Document = string() | binary()
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options; see
%% {@link start_parser/1} for any related informations.

parse_document(Document) ->
    parse_document(Document, []).

%% @spec (Document, Parser_Options) -> [XML_Element] | done
%%     Document = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser; see {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

parse_document(Document, Parser_Options) ->
    Parser = start_parser(Parser_Options),
    try
        parse_final(Parser, Document)
    catch
        throw:Exception ->
            throw(Exception)
    after
        stop_parser(Parser)
    end.

%% @spec (Fragment) -> [XML_Element]
%%     Fragment = string() | binary()
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%% @doc Parse a fragment of an XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options,
%% but will set `no_root_depth' (which can be overriden); see {@link
%% start_parser/1} for any related informations.

parse_document_fragment(Fragment) ->
    parse_document_fragment(Fragment, []).

%% @spec (Fragment, Parser_Options) -> [XML_Element]
%%     Fragment = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlnselement() | xmlelement() | xmlnsendelement() | xmlendelement()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser but `no_root_depth' is prepended (this can be overriden); see
%% {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

parse_document_fragment(Fragment, Parser_Options) ->
    Parser = start_parser([no_root_depth | Parser_Options]),
    try
        parse(Parser, Fragment)
    catch
        throw:Exception ->
            throw(Exception)
    after
        stop_parser(Parser)
    end.

%% @hidden

port_revision(#xml_parser{port = Port} = _Parser) ->
    {ok, Result} = binary_to_term(port_control(Port,
        ?EXPAT_SVN_REVISION, <<>>)),
    Result.

% --------------------------------------------------------------------
% Functions to handle XML attributes (xmlnsattribute() & xmlattribute()).
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (Attrs, Attr_Name) -> Attr | undefined
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name' from the list.
%%
%% Return `undefined' if the attribute isn't found.

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
    undefined.

%% @spec (Attrs, NS, Attr_Name) -> Attr | undefined
%%     Attrs = [xmlnsattribute()]
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute()
%% @doc Return the attribute named `Attr_Name' from the list with the
%% `NS' namespace URI.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node_from_list([Attr | Rest], NS, Name) ->
    case Attr of
        #xmlattr{ns = NS, name = Name} ->
            Attr;
        _ ->
            get_attribute_node_from_list(Rest, NS, Name)
    end;
get_attribute_node_from_list([], _NS, _Name) ->
    undefined.

%% @spec (XML_Element, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name'.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlnselement() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute()
%% @doc Return the attribute named `Attr_Name' with the `NS' namespace URI.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
    get_attribute_node_from_list(Attrs, NS, Name);
get_attribute_node(undefined, _NS, _Name) ->
    undefined.

%% @spec (Attrs, Attr_Name) -> Attr_Value | nil()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
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
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
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
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name'.
%%
%% Return an empty string if the attribute isn't found.

get_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_from_list(Attrs, Name);
get_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_from_list(Attrs, Name);
get_attribute(undefined, _Name) ->
    "".

%% @spec (XML_Element, NS, Attr_Name) -> Attr_Value | nil()
%%     XML_Element = xmlnselement() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI.
%%
%% Return an empty string if the attribute isn't found.

get_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
    get_attribute_from_list(Attrs, NS, Name);
get_attribute(undefined, _NS, _Name) ->
    "".

%% @spec (Attrs, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%% @doc Check the presence of attribute `Attr_Name' in the list.

has_attribute_in_list(Attrs, Name) ->
    case get_attribute_node_from_list(Attrs, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (Attrs, NS, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%% @doc Check the presence of attribute `Attr_Name' with namespace `NS'
%% in the list.

has_attribute_in_list(Attrs, NS, Name) ->
    case get_attribute_node_from_list(Attrs, NS, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, Attr_Name) -> bool()
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%% @doc Check the presence of attribute `Attr_Name' in the XML element.

has_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(undefined, _Name) ->
    false.

%% @spec (XML_Element, NS, Attr_Name) -> bool()
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%% @doc Check the presence of attribute `Attr_Name' with namespace `NS'
%% in the XML element.

has_attribute(#xmlnselement{attrs = Attrs} = _XML_Element, NS, Name) ->
    has_attribute_in_list(Attrs, NS, Name);
has_attribute(undefined, _NS, _Name) ->
    false.

%% @spec (Attrs, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
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
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
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
%%     XML_Element = xmlnselement() | xmlelement()
%%     Attr_Name = atom() | string()
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
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
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
%%       NS = atom() | string()
%%       Name = atom() | string()
%%       Value = string()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Set multiple attributes at a time.
%%
%% Existing attributes are not completly overwritten by the ones present
%% in `Attrs_Spec'. They are simply updated.

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
%%     Attr_Name = atom() | string()
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
%%     Attr_Name = atom() | string()
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
%%     Attr_Name = atom() | string()
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
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     New_XML_Element = xmlnselement()
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute(#xmlnselement{attrs = Attrs} = XML_Element, NS, Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, NS, Name),
    XML_Element#xmlnselement{attrs = New_Attrs}.

% --------------------------------------------------------------------
% Functions to handle XML elements (xmlnselement() & xmlelement()).
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (XML_Element, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlnselement() | xmlelement()
%% @doc Search in the children of `XML_Element' an element named `Name'.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

get_element_by_name(#xmlnselement{children = Children}, Name) ->
    get_element_by_name2(Children, Name);
get_element_by_name(#xmlelement{children = Children}, Name) ->
    get_element_by_name2(Children, Name);
get_element_by_name(undefined, _Name) ->
    undefined.

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
    undefined;
get_element_by_name2(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlnselement() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlnselement()
%% @doc Search in the children of `XML_Element' an element named `Name'
%% with `NS' namespace URI.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

get_element_by_name(#xmlnselement{children = Children}, NS, Name) ->
    get_element_by_name2(Children, NS, Name);
get_element_by_name(undefined, _NS, _Name) ->
    undefined.

get_element_by_name2([Node | Rest], NS, Name) ->
    case Node of
        #xmlnselement{ns = NS, name = Name} ->
            Node;
        _ ->
            get_element_by_name2(Rest, NS, Name)
    end;
get_element_by_name2([], _NS, _Name) ->
    undefined;
get_element_by_name2(undefined, _NS, _Name) ->
    undefined.

%% @spec (XML_Element, Name) -> [XML_Subelement]
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlnselement() | xmlelement()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name'
%%
%% This will only search among direct children.

get_elements_by_name(#xmlnselement{children = Children}, Name) ->
    get_elements_by_name2(Children, Name);
get_elements_by_name(#xmlelement{children = Children}, Name) ->
    get_elements_by_name2(Children, Name);
get_elements_by_name(undefined, _Name) ->
    [].

get_elements_by_name2(undefined, _Name) ->
    [];
get_elements_by_name2([], _Name) ->
    [];
get_elements_by_name2(Children, Name) ->
    lists:filter(filter_by_name(Name), Children).

filter_by_name(Searched_Name) ->
    fun
    (#xmlelement{name = Name}) when Name == Searched_Name   -> true;
    (#xmlnselement{name = Name}) when Name == Searched_Name -> true;
    (_)                                                     -> false
    end.

%% @spec (XML_Element, NS, Name) -> [XML_Subelement]
%%     XML_Element = xmlnselement() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlnselement()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name' with `NS' namespace URI.
%%
%% This will only search among direct children.

get_elements_by_name(#xmlnselement{children = Children}, NS, Name) ->
    get_elements_by_name2(Children, NS, Name);
get_elements_by_name(undefined, _NS, _Name) ->
    [].

get_elements_by_name2(undefined, _NS, _Name) ->
    [];
get_elements_by_name2([], _NS, _Name) ->
    [];
get_elements_by_name2(Children, NS, Name) ->
    lists:filter(filter_by_name(NS, Name), Children).

filter_by_name(Searched_NS, Searched_Name) ->
    fun
    (#xmlnselement{name = Name, ns = NS})
      when NS == Searched_NS, Name == Searched_Name ->
        true;
    (_) ->
        false
    end.

%% @spec (XML_Element, NS) -> XML_Subelement | undefined
%%     XML_Element = xmlnselement() | undefined
%%     NS = atom() | string()
%%     XML_Subelement = xmlnselement()
%% @doc Search in the children of `XML_Element' the first element with
%% `NS' namespace URI.
%%
%% If no element with the given namespace is found, it returns
%% `undefined'. This will only search among direct children.
%%
%% This function is particularly usefull to extract XMPP error codes.

get_element_by_ns(#xmlnselement{children = Children}, NS) ->
    get_element_by_ns2(Children, NS);
get_element_by_ns(undefined, _NS) ->
    undefined.

get_element_by_ns2([Node | Rest], NS) ->
    case Node of
        #xmlnselement{ns = NS} ->
            Node;
        _ ->
            get_element_by_ns2(Rest, NS)
    end;
get_element_by_ns2([], _NS) ->
    undefined;
get_element_by_ns2(undefined, _NS) ->
    undefined.

%% @spec (XML_Element) -> [XML_Subelement]
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     XML_Subelement = xmlnselement() | xmlelement()
%% @doc Get all the element children of the given element, skipping
%% non-element nodes likes cdata.

get_child_elements(#xmlnselement{children = Children}) ->
    get_child_elements2(Children);
get_child_elements(#xmlelement{children = Children}) ->
    get_child_elements2(Children);
get_child_elements(undefined) ->
    [].

get_child_elements2(undefined) ->
    [];
get_child_elements2([]) ->
    [];
get_child_elements2(Children) ->
    lists:filter(fun is_element/1, Children).

is_element(#xmlelement{})   -> true;
is_element(#xmlnselement{}) -> true;
is_element(_)               -> false.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Child = xmlnselement() | xmlelement() | xmlcdata()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Prepend `Child' to `XML_Element''s children list.

prepend_child(#xmlnselement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlnselement{children = New_Children};
prepend_child(#xmlelement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlelement{children = New_Children};
prepend_child(#xmlnselement{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlnselement{children = New_Children};
prepend_child(#xmlelement{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Prepend every `Children' to `XML_Element''s children list.

prepend_children(#xmlnselement{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlnselement{children = New_Children};
prepend_children(#xmlelement{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlelement{children = New_Children};
prepend_children(#xmlnselement{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlnselement{children = Concat_Children};
prepend_children(#xmlelement{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlelement{children = Concat_Children}.

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
%% @doc Replace `Old_Child' by `New_Child' in `XML_Element' children
%% list.

replace_child(#xmlnselement{children = Children} = XML_Element,
  Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlnselement{children = New_Children};
replace_child(#xmlelement{children = Children} = XML_Element,
  Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlelement{children = New_Children}.

replace_child2(undefined, _Old_Child, _New_Child) ->
    undefined;
replace_child2([], _Old_Child, _New_Child) ->
    [];
replace_child2(Children, Old_Child, New_Child) ->
    [case C of
      Old_Child -> New_Child;
      _         -> C
      end || C <- Children].

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

% --------------------------------------------------------------------
% Functions to handle XML text nodes.
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (Children) -> CData
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%%     CData = binary()
%% @doc Concatenate and return any character data from the given
%% children list.

get_cdata_from_list(undefined) ->
    <<>>;
get_cdata_from_list(Children) ->
    % The function list_to_binary/1 will concatenate every
    % binaries in the list returned by get_cdata_from_list2/2.
    list_to_binary(get_cdata_from_list2(Children, [])).

get_cdata_from_list2([#xmlcdata{cdata = Chunk} | Rest], Data) ->
    get_cdata_from_list2(Rest, [Chunk | Data]);
get_cdata_from_list2([_ | Rest], Data) ->
    get_cdata_from_list2(Rest, Data);
get_cdata_from_list2([], Data) ->
    lists:reverse(Data).

%% @spec (Children) -> CData
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%%     CData = string()
%% @doc Concatenate and return any character data from the given
%% children list.

get_cdata_from_list_as_list(Children) ->
    binary_to_list(get_cdata_from_list(Children)).

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     CData = binary()
%% @doc Concatenate and return any character data of the given XML
%% element.
%%
%% This function is `get_tag_cdata/1' renamed in `get_cdata/1'. It
%% doesn't take a list of children like the old `get_cdata/1', use
%% {@link get_cdata_from_list/1} for this purpose!

get_cdata(#xmlnselement{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(#xmlelement{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(undefined) ->
    % This clause makes it possible to write code like:
    % exmpp_xml:get_cdata(exmpp_xml:get_element_by_name(XML_El, body))
    <<>>.

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlnselement() | xmlelement() | undefined
%%     CData = string()
%% @doc Concatenate and return any character data of the given XML
%% element.

get_cdata_as_list(XML_Element) ->
    binary_to_list(get_cdata(XML_Element)).

%% @spec (Children) -> New_Children
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%%     New_Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one.

normalize_cdata_in_list(undefined) ->
    undefined;
normalize_cdata_in_list([]) ->
    [];
normalize_cdata_in_list(Children) ->
    normalize_cdata_in_list2(Children, [], []).

normalize_cdata_in_list2([], Current_CDatas, New_Children) ->
    New_Children1 = case list_to_binary(lists:reverse(Current_CDatas)) of
        <<>>  -> [New_Children];
        CData -> [#xmlcdata{cdata = CData} | New_Children]
    end,
    lists:reverse(lists:flatten(New_Children1));
normalize_cdata_in_list2([#xmlcdata{cdata = CData} | Rest], Current_CDatas,
  New_Children) ->
    normalize_cdata_in_list2(Rest, [CData | Current_CDatas], New_Children);
normalize_cdata_in_list2([XML_Node | Rest], Current_CDatas, New_Children) ->
    New_Children1 = case list_to_binary(lists:reverse(Current_CDatas)) of
        <<>>  -> [XML_Node | New_Children];
        CData -> [XML_Node, #xmlcdata{cdata = CData} | New_Children]
    end,
    normalize_cdata_in_list2(Rest, [], New_Children1).

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlnselement() | xmlelement()
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one.
%%
%% One caveats is the reconstructed {@link xmlcdata()} is appended at
%% the end of the children list.

normalize_cdata(#xmlnselement{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlnselement{children = New_Children};
normalize_cdata(#xmlelement{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Children, CData) -> New_Children
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%%     CData = binary() | string()
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
%%     CData = binary() | string()
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
%%     Children = [xmlnselement() | xmlelement() | xmlcdata()] | undefined
%%     New_Children = [xmlnselement() | xmlelement()] | undefined
%% @doc Remove any character data from the given XML element children
%% list.

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
%%     CData = binary()
%%     Not_Found = nil() | <<>> | undefined
%% @throws {xml, path, ending_component_not_at_the_end, Path} |
%%         {xml, path, invalid_component,               Path}
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
        undefined      -> get_path_not_found(Path);
        XML_Subelement -> get_path(XML_Subelement, Path)
    end;
get_path(XML_Element, [{element, NS, Name} | Path]) ->
    case get_element_by_name(XML_Element, NS, Name) of
        undefined      -> get_path_not_found(Path);
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
get_path(_XML_Element, [{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, Path) ->
    throw({xml, path, invalid_component, Path}).

get_path_not_found([{element, _Name} | Rest]) ->
    get_path_not_found(Rest);
get_path_not_found([{attribute, _Name}]) ->
    "";
get_path_not_found([{attribute, _NS, _Name}]) ->
    "";
get_path_not_found([cdata]) ->
    <<>>;
get_path_not_found([]) ->
    undefined;
get_path_not_found([{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found([cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found(Path) ->
    throw({xml, path, invalid_component, Path}).

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
%%     NS = atom() | string()
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

xmlnselement_to_xmlelement(#xmlnselement{ns = NS, name = Name, attrs = Attrs,
  declared_ns = Declared_NS, children = Children}, Default_NS, Prefixed_NS) ->
    % First, we add namespace declarations to element attributes.
    {Prefix, Attrs1, Default_NS1, Prefixed_NS1} = forward_declare_ns(NS,
      Declared_NS, Attrs, Default_NS, Prefixed_NS),
    % Then, we convert attributes ot the old format.
    {New_Attrs, Prefixed_NS2} = xmlnsattributes_to_xmlattributes(Attrs1,
      Prefixed_NS1),
    % We can now proceed with the modification of the name.
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    New_Name = case Prefix of
        none -> Name_S;
        _    -> Prefix ++ ":" ++ Name_S
    end,
    % Treat children.
    New_Children = xmlnselements_to_xmlelements(Children,
      Default_NS1, Prefixed_NS2),
    % Now, create the final #xmlelement.
    #xmlelement{name = New_Name, attrs = New_Attrs, children = New_Children};
xmlnselement_to_xmlelement(#xmlnsendelement{ns = NS, name = Name,
  prefix = Wanted_Prefix}, Default_NS, Prefixed_NS) ->
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    New_Name = case Default_NS of
        [NS | _] ->
            % This closing tag uses the default namespace.
            Name_S;
        _ ->
            % Search a prefix in already declared namespaces.
            case search_in_prefixed_ns(NS, Prefixed_NS) of
                undefined  when Wanted_Prefix /= undefined ->
                    Wanted_Prefix ++ ":" ++ Name_S;
                undefined ->
                    % Too late to declare something; the
                    % namespace should have been provided
                    % by the caller.
                    Name_S;
                Prefix ->
                    Prefix ++ ":" ++ Name_S
            end
    end,
    #xmlendelement{name = New_Name};
xmlnselement_to_xmlelement(XML_El, _Default_NS, _Prefixed_NS) ->
    % xmlelement() or xmlcdata().
    XML_El.

% Function called to convert element attributes.
xmlnsattributes_to_xmlattributes(Attrs, Prefixed_NS) ->
    xmlnsattributes_to_xmlattributes2(Attrs, Prefixed_NS, []).

xmlnsattributes_to_xmlattributes2([#xmlattr{ns = NS, name = Name,
  value = Value, prefix = Wanted_Prefix} | Rest],
  Prefixed_NS, Converted_Attrs) ->
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    {New_Name, Converted_Attrs1, Prefixed_NS1} = case NS of
        undefined ->
            {
              Name_S,
              Converted_Attrs,
              Prefixed_NS
            };
        _ ->
            case search_in_prefixed_ns(NS, Prefixed_NS) of
                undefined ->
                    % Never declared.
                    Prefix = case Wanted_Prefix of
                        undefined ->
                            % Doesn't provide a prefix, it must be generated.
                            % FIXME Generate a random prefix.
                            new_auto_prefix(Prefixed_NS);
                        _ ->
                            % Use the desired prefix.
                            Wanted_Prefix
                    end,
                    NS_S = if
                        is_atom(NS) -> atom_to_list(NS);
                        true        -> NS
                    end,
                    NS_Decl = {"xmlns:" ++ Prefix, NS_S},
                    {
                      Prefix ++ ":" ++ Name_S,
                      Converted_Attrs ++ [NS_Decl],
                      [{NS, Prefix} | Prefixed_NS]
                    };
                Prefix ->
                    % Use an already declared prefix.
                    {
                      Prefix ++ ":" ++ Name_S,
                      Converted_Attrs,
                      Prefixed_NS
                    }
            end
    end,
    xmlnsattributes_to_xmlattributes2(Rest, Prefixed_NS1,
      [{New_Name, Value} | Converted_Attrs1]);
xmlnsattributes_to_xmlattributes2([], Prefixed_NS, Converted_Attrs) ->
    {lists:reverse(Converted_Attrs), Prefixed_NS}.

% Function called to convert element's children.
xmlnselements_to_xmlelements(undefined, _Default_NS, _Prefixed_NS) ->
    undefined;
xmlnselements_to_xmlelements([], _Default_NS, _Prefixed_NS) ->
    [];
xmlnselements_to_xmlelements(XML_Elements, Default_NS, Prefixed_NS) ->
    xmlnselements_to_xmlelements2(XML_Elements, [], Default_NS, Prefixed_NS).

xmlnselements_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
  Default_NS, Prefixed_NS) ->
    XML_Element = xmlnselement_to_xmlelement(XML_NS_Element,
      Default_NS, Prefixed_NS),
    xmlnselements_to_xmlelements2(Rest,
      [XML_Element | XML_Elements], Default_NS, Prefixed_NS);
xmlnselements_to_xmlelements2([], XML_Elements, _Default_NS, _Prefixed_NS) ->
    lists:reverse(XML_Elements).

% Helpers.
search_in_prefixed_ns(NS, Prefixed_NS) ->
    case lists:keysearch(NS, 1, Prefixed_NS) of
        {value, {_NS, Prefix}} ->
            Prefix;
        _ ->
            case lists:keysearch(NS, 1, ?IMPLICIT_PREFIXED_NS) of
                {value, {_NS, Prefix}} ->
                    Prefix;
                _ ->
                    undefined
            end
    end.

forward_declare_ns(Curr_NS, [{NS, none} | Rest],
  Attrs, Default_NS, Prefixed_NS) ->
    % Forward-declare a default namespace.
    NS_S = if
        is_atom(NS) -> atom_to_list(NS);
        true        -> NS
    end,
    NS_Decl = #xmlattr{name = "xmlns", value = NS_S},
    New_Attrs = [NS_Decl | Attrs],
    New_Default_NS = [NS | Default_NS],
    forward_declare_ns(Curr_NS, Rest, New_Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(Curr_NS, [{NS, Prefix} = PNS | Rest],
  Attrs, Default_NS, Prefixed_NS) ->
    case lists:member(PNS, ?IMPLICIT_PREFIXED_NS) of
        true ->
            % This is an implicitly declared namespace (with the same
            % prefix). We do not re-declare it.
            forward_declare_ns(Curr_NS, Rest, Attrs,
              Default_NS, Prefixed_NS);
        _ ->
            % Forward-declare a prefixed namespace.
            NS_S = if
                is_atom(NS) -> atom_to_list(NS);
                true        -> NS
            end,
            NS_Decl = #xmlattr{name = "xmlns:" ++ Prefix, value = NS_S},
            New_Attrs = [NS_Decl | Attrs],
            Prefixed_NS1 = [PNS | Prefixed_NS],
            forward_declare_ns(Curr_NS, Rest, New_Attrs,
              Default_NS, Prefixed_NS1)
    end;
forward_declare_ns(undefined, [], Attrs, Default_NS, Prefixed_NS) ->
    {none, Attrs, Default_NS, Prefixed_NS};
forward_declare_ns(Curr_NS, [], Attrs, Default_NS, Prefixed_NS) ->
    % We finish with the current namespace of the element.
    case Default_NS of
        [Curr_NS | _] ->
            % The element belongs to the current default namespace.
            % There's nothing to do.
            {none, Attrs, Default_NS, Prefixed_NS};
        _ ->
            % We look for a prefixed namespace.
            case search_in_prefixed_ns(Curr_NS, Prefixed_NS) of
                undefined ->
                    % This element uses a new namespace: it'll become
                    % the new default one.
                    Curr_NS_S = if
                        is_atom(Curr_NS) -> atom_to_list(Curr_NS);
                        true             -> Curr_NS
                    end,
                    NS_Decl = #xmlattr{name = "xmlns", value = Curr_NS_S},
                    New_Attrs = [NS_Decl | Attrs],
                    Default_NS1 = [Curr_NS | Default_NS],
                    {none, New_Attrs, Default_NS1, Prefixed_NS};
                Prefix ->
                    % Found one: we return the corresponding prefix.
                    {Prefix, Attrs, Default_NS, Prefixed_NS}
            end
    end.

new_auto_prefix(Prefixed_NS) ->
    new_auto_prefix2(Prefixed_NS, 1).

new_auto_prefix2(Prefixed_NS, Seq) ->
    Prefix = "ns" ++ integer_to_list(Seq),
    case lists:keymember(Prefix, 2, Prefixed_NS) of
        true  -> new_auto_prefix2(Prefixed_NS, Seq + 1);
        false -> Prefix
    end.

%% @spec (XML_Element) -> XML_NS_Element
%%     XML_Element = xmlelement() | xmlcdata()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()}
%% tuple.
%%
%% Other tuples are ignored.

xmlelement_to_xmlnselement(XML_Element) ->
    xmlelement_to_xmlnselement(XML_Element, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_NS_Element
%%     XML_Element = xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()}
%% tuple.
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
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     New_Default_NS = [NS]
%%     New_Prefixed_NS = [{NS, Prefix}]
%% @doc Convert an {@link xmlelement()} to an {@link xmlnselement()}
%% tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlnselement_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.
%%
%% This function will returned updated namespaces tables
%% `New_Default_NS' and `New_Prefixed_NS' which can be used for future
%% calls.

xmlelement_to_xmlnselement_and_ns_tables(
  #xmlelement{name = Name, attrs = Attrs, children = Children},
  Default_NS, Prefixed_NS) ->
    % Udpate NS tables by looking at each attribute for NS declarations.
    % These later are removed at the same time.
    {Declared_NS, Attrs1, Default_NS1, Prefixed_NS1} =
      update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS),
    % Convert attributes and children to the new format.
    New_Attrs = xmlattributes_to_xmlnsattributes(Attrs1, Prefixed_NS1),
    New_Children = xmlelements_to_xmlnselements(Children,
      Default_NS1, Prefixed_NS1),
    % Check the element namespace and convert it to the new format.
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    XML_NS_Element = case string:tokens(Name_S, ":") of
        [Prefix, Real_Name] ->
            Real_Name_A = list_to_atom(Real_Name),
            case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS1) of
                undefined ->
                    % Namespace never declared.
                    #xmlnselement{
                      ns = undefined,
                      declared_ns = Declared_NS,
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    };
                NS ->
                    #xmlnselement{
                      ns = NS,
                      declared_ns = Declared_NS,
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    }
            end;
        [Real_Name] ->
            Real_Name_A = list_to_atom(Real_Name),
            case Default_NS1 of
                [NS | _] ->
                    % Uses the current default namespace.
                    #xmlnselement{
                      ns = NS,
                      declared_ns = lists:delete({NS, none}, Declared_NS),
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    };
                _ ->
                    % No default namespace declared.
                    #xmlnselement{
                      ns = undefined,
                      declared_ns = Declared_NS,
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    }
            end
    end,
    {XML_NS_Element, Default_NS1, Prefixed_NS1};
xmlelement_to_xmlnselement_and_ns_tables(
  #xmlendelement{name = Name}, Default_NS, Prefixed_NS) ->
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    XML_NS_Element = case string:tokens(Name_S, ":") of
        [Prefix, Real_Name] ->
            Real_Name_A = list_to_atom(Real_Name),
            case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) of
                undefined ->
                    % Namespace never declared.
                    #xmlnsendelement{
                      ns = undefined,
                      prefix = Prefix,
                      name = Real_Name_A
                    };
                NS ->
                    #xmlnsendelement{
                      ns = NS,
                      prefix = Prefix,
                      name = Real_Name_A
                    }
            end;
        [Real_Name] ->
            Real_Name_A = list_to_atom(Real_Name),
            case Default_NS of
                [NS | _] ->
                    % Uses the current default namespace.
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

% Function called to extract namespaces and their prefix (if any).
update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS) ->
    update_ns_from_xmlattributes2(Attrs, Default_NS, Prefixed_NS, [], []).

update_ns_from_xmlattributes2([{Name, Value} = Attr | Rest],
  Default_NS, Prefixed_NS, Declared_NS, Purged_Attrs) ->
    case string:tokens(Name, ":") of
        ["xmlns"] ->
            % Default NS declaration.
            update_ns_from_xmlattributes2(Rest,
              [list_to_atom(Value) | Default_NS],
              Prefixed_NS,
              [{list_to_atom(Value), none} | Declared_NS],
              Purged_Attrs);
        ["xmlns", Prefix] ->
            % Prefixed NS declaration.
            update_ns_from_xmlattributes2(Rest,
              Default_NS,
              [{list_to_atom(Value), Prefix} | Prefixed_NS],
              [{list_to_atom(Value), Prefix} | Declared_NS],
              Purged_Attrs);
        _ ->
            % Irrelevant attribute.
            update_ns_from_xmlattributes2(Rest,
              Default_NS, Prefixed_NS, Declared_NS, [Purged_Attrs | Attr])
    end;
update_ns_from_xmlattributes2([], Default_NS, Prefixed_NS,
  Declared_NS, Purged_Attrs) ->
    {Declared_NS, lists:reverse(Purged_Attrs), Default_NS, Prefixed_NS}.

% Function called to convert element's attributes.
xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS) ->
    xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS, []).

xmlattributes_to_xmlnsattributes([{Name, Value} | Rest],
  Prefixed_NS, Converted_Attrs) ->
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    New_Attr = case string:tokens(Name_S, ":") of
        [Prefix, Real_Name] ->
            Real_Name_A = list_to_atom(Real_Name),
            case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) of
                undefined ->
                    % Namespace never declared.
                    #xmlattr{
                      ns = undefined,
                      prefix = Prefix,
                      name = Real_Name_A,
                      value = Value
                    };
                NS ->
                    #xmlattr{
                      ns = NS,
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

% Function called to convert element's children.
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

% Helpers.
search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) ->
    case lists:keysearch(Prefix, 2, Prefixed_NS) of
        {value, {NS, _Prefix}} ->
            NS;
        _ ->
            case lists:keysearch(Prefix, 2, ?IMPLICIT_PREFIXED_NS) of
                {value, {NS, _Prefix}} ->
                    NS;
                _ ->
                    undefined
            end
    end.

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
                    [$<, Name_S, attrs_to_list(Attrs), $/, $>];
                _ ->
                    % NS stacks are passed to
                    % document_fragment_to_list/3
                    % again, but this isn't relevant
                    % without namespace support.
                    Norm = normalize_cdata_in_list(Els),
                    [$<, Name_S, attrs_to_list(Attrs), $>,
                      [document_fragment_to_list(E, Default_NS, Prefixed_NS) ||
                        E <- Norm],
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
        #xmlcdata{cdata = CData} ->
            binary_to_list(?ESCAPE(CData))
    end.

attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $", escape_using_entities(Value), $"].

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

escape_using_entities(CData) when is_list(CData) ->
    lists:flatten([case C of
      $& -> "&amp;";
      $< -> "&lt;";
      $> -> "&gt;";
      $" -> "&quot;";
      $' -> "&apos;";
      _  -> C
    end || C <- CData]);

escape_using_entities(CData) when is_binary(CData) ->
    escape_using_entities2(CData, []).

escape_using_entities2(<<C:8, Rest/binary>>, New_CData) ->
    New_C = case C of
        $& -> <<"&amp;">>;
        $< -> <<"&lt;">>;
        $> -> <<"&gt;">>;
        $" -> <<"&quot;">>; % "
        $' -> <<"&apos;">>; % '
        _  -> C
    end,
    escape_using_entities2(Rest, [New_C | New_CData]);
escape_using_entities2(<<>>, New_CData) ->
    list_to_binary(lists:reverse(New_CData)).

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @doc Escape text using CDATA sections.

escape_using_cdata(CData) when is_list(CData) ->
    escape_using_cdata_list(CData, false, []);
escape_using_cdata(CData) when is_binary(CData) ->
    case cdata_need_escape(CData) of
        no ->
            CData;
        global ->
            list_to_binary([<<"<![CDATA[">>, CData, <<"]]>">>]);
        {split, End_Token_Positions} ->
            Escaped = escape_using_cdata_binary(CData, End_Token_Positions),
            list_to_binary(Escaped)
    end.

% If a text node contains the characters '<' or '&', it must be enclosed
% inside CDATA sections. If such a text also contains CDATA end token
% ("]]>"), it must be split in multiple CDATA sections.
%
% See:
%   http://www.w3.org/TR/xml11/#syntax
%   http://en.wikipedia.org/wiki/CDATA#Uses_of_CDATA_sections
%
% For binary(), we do it in two steps (first, is it needed, then do it).
% This is because in most cases, the text node won't have CDATA end token.
% XXX Should we do the same for lists?

escape_using_cdata_list([], false, Escaped) ->
    lists:reverse(Escaped);
escape_using_cdata_list([], true, Escaped) ->
    "<![CDATA[" ++ lists:reverse(lists:flatten(Escaped)) ++ "]]>";
escape_using_cdata_list([$], $], $> | Rest], _Must_Escape, Escaped) ->
    escape_using_cdata_list(Rest, true, [">[ATADC[!<>]]]]" | Escaped]);
escape_using_cdata_list([C | Rest], _Must_Escape, Escaped)
  when C == $<; C == $& ->
    escape_using_cdata_list(Rest, true, [C | Escaped]);
escape_using_cdata_list([C | Rest], Must_Escape, Escaped) ->
    escape_using_cdata_list(Rest, Must_Escape, [C | Escaped]).

% This function returns what kind of escape must be done:
%   . 'no'
%   . 'global' for text containing '<' and '&'
%   . {'split, End_Token_Pos} for text containing CDATA end token(s)

cdata_need_escape(CData) ->
    cdata_need_escape2(CData, 0, false, []).

cdata_need_escape2(<<>>, _Current_Pos, false, _End_Token_Pos) ->
    no;
cdata_need_escape2(<<>>, _Current_Pos, true, []) ->
    global;
cdata_need_escape2(<<>>, _Current_Pos, true, End_Token_Pos) ->
    {split, lists:reverse(End_Token_Pos)};
cdata_need_escape2(<<$], $], $>, Rest/binary>>, Current_Pos, _Must_Escape,
  End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 3, true,
      [Current_Pos + 1 | End_Token_Pos]);
cdata_need_escape2(<<$<, Rest/binary>>, Current_Pos, _Must_Escape,
  End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, true, End_Token_Pos);
cdata_need_escape2(<<$&, Rest/binary>>, Current_Pos, _Must_Escape,
  End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, true, End_Token_Pos);
cdata_need_escape2(<<_:8, Rest/binary>>, Current_Pos, Must_Escape,
  End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, Must_Escape, End_Token_Pos).

% This function use the End_Token_Pos list returned by
% cdata_need_escape/1 and split CDATA end tokens at those positions.

escape_using_cdata_binary(CData, End_Token_Pos) ->
    escape_using_cdata_binary2(CData, 0, End_Token_Pos, []).

escape_using_cdata_binary2(Rest, _Current_Pos, [], Escaped) ->
    lists:reverse([<<"]]>">>, Rest, <<"<![CDATA[">> | Escaped]);
escape_using_cdata_binary2(CData, Current_Pos, [Pos | End_Token_Pos],
  Escaped) ->
    Split = Pos - Current_Pos,
    {CData1, CData2} = split_binary(CData, Split + 1),
    escape_using_cdata_binary2(CData2, Pos + 1, End_Token_Pos,
      [<<"]]>">>, CData1, <<"<![CDATA[">> | Escaped]).

%% @spec () -> escape_using_entities | escape_using_cdata
%% @doc Tell what escaping function will be used internally.

-ifdef(ESCAPE_USING_CDATA_SECTIONS).
internal_escaping_function_name() ->
    escape_using_cdata.
-else.
internal_escaping_function_name() ->
    escape_using_entities.
-endif.

% --------------------------------------------------------------------
% Utilities.
% --------------------------------------------------------------------

handle_options([namespace | Rest], #xml_parser{port = P} = Parser) ->
    Ret = port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(true)),
    case binary_to_term(Ret) of
        ok              -> handle_options(Rest, Parser);
        {error, Reason} -> {error, init, Reason}
    end;
handle_options([no_namespace | Rest], #xml_parser{port = P} = Parser) ->
    Ret = port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(false)),
    case binary_to_term(Ret) of
        ok              -> handle_options(Rest, Parser);
        {error, Reason} -> {error, init, Reason}
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

handle_options([Invalid_Option | _Rest], _Parser) ->
    {error, invalid, Invalid_Option};

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
%%     Name = string()
%%     Attrs = [xmlattribute()]
%%     Children = [xmlelement() | xmlcdata()] | undefined.
%% Record representing an XML tag.

%% @type xmlnselement() = {xmlnselement, NS, Name, Attrs, Children}
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     Attrs = [xmlnsattribute()]
%%     Children = [xmlnselement() | xmlcdata()] | undefined.
%% Record representing an XML tag when namespace support is enabled.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlattribute() = {Name, Value}
%%     Name = atom() | string()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlnsattribute() = {xmlattr, NS, Name, Value}
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlendelement() = {xmlendelement, Name}
%%     Name = string().
%% Record representing an XML closing tag for nodes above the configured
%% `root_depth' (see {@link xmlparseroption()}).

%% @type xmlnsendelement() = {xmlnsendelement, NS, Name}
%%     NS = atom() | string()
%%     Name = atom() | string().
%% Record representing an XML closing tag when namespace support is
%% enabled, for nodes above the configured `root_depth' (see {@link
%% xmlparseroption()}).

%% @type pathcomponent() = {element, Elem_Name} | {element, NS, Elem_Name} | {attribute, Attr_Name} | {attribute, NS, Attr_Name} | cdata
%%     NS = atom() | string()
%%     Elem_Name = atom() | string()
%%     Attr_Name = atom() | string().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.
