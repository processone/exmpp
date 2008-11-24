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
-vsn('$Revision$ ').

-behaviour(gen_server).

-include("exmpp.hrl").

% Deprecated functions list.
-deprecated({get_element_by_name, 2, next_major_release}).
-deprecated({get_element_by_name, 3, next_major_release}).
-deprecated({get_elements_by_name, 2, next_major_release}).
-deprecated({get_elements_by_name, 3, next_major_release}).

% Initialization.
-export([
  start/0,
  start_link/0
]).

% Registry handling.
-export([
  register_engine/2,
  register_engine/3,
  get_engine_names/0,
  is_engine_available/1,
  get_engine_driver/1
]).

% Parser.
-export([
  start_parser/0,
  start_parser/1,
  reset_parser/1,
  reset_parser/2,
  stop_parser/1,
  add_known_nss/2,
  add_known_names/2,
  add_known_attrs/2,
  add_autoload_known_nss/1,
  add_autoload_known_names/1,
  add_autoload_known_attrs/1,
  parse/2,
  parse_final/2,
  parse_document/1,
  parse_document/2,
  parse_document_fragment/1,
  parse_document_fragment/2,
  port_revision/1
]).

% Namespace handling.
-export([
  is_ns_declared_here/2,
  declare_ns_here/3,
  get_ns_as_list/1,
  get_ns_as_atom/1
]).

% Attribute handling.
-export([
  attribute_matches/2,
  attribute_matches/3,
  make_attribute/1,
  make_attribute/2,
  get_attribute_node_from_list/2,
  get_attribute_node_from_list/3,
  get_attribute_node/2,
  get_attribute_node/3,
  get_attribute_from_list/3,
  get_attribute_from_list/4,
  get_attribute/3,
  get_attribute/4,
  set_attribute_in_list/2,
  set_attribute_in_list/3,
  set_attribute_in_list/4,
  set_attribute/2,
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
  get_name_as_list/1,
  get_name_as_atom/1,
  element_matches/2,
  element_matches/3,
  element_matches_by_ns/2,
  make_element/1,
  make_element/2,
  get_element/2,
  get_element/3,
  get_elements/3,
  get_elements/2,
  get_element_by_name/2,  % Deprecated
  get_element_by_name/3,  % Deprecated
  get_elements_by_name/3, % Deprecated
  get_elements_by_name/2, % Deprecated
  get_element_by_ns/2,
  has_element/2,
  has_element/3,
  has_element_by_ns/2,
  get_child_elements/1,
  remove_element/2,
  remove_element/3,
  remove_element_by_ns/2,
  remove_elements/2,
  remove_elements/3,
  remove_elements_by_ns/2
]).
-export([
  prepend_child/2,
  prepend_children/2,
  append_child/2,
  append_children/2,
  replace_child/3,
  set_children/2,
  filter/2,
  fold/3,
  foreach/2,
  map/2
]).

% Character data handling.
-export([
  cdata/1,
  get_cdata_from_list/1,
  get_cdata_from_list_as_list/1,
  get_cdata/1,
  get_cdata_as_list/1,
  normalize_cdata_in_list/1,
  normalize_cdata/1,
  set_cdata_in_list/2,
  set_cdata/2,
  append_cdata/2,
  remove_cdata_from_list/1,
  remove_cdata/1,
  is_whitespace/1,
  remove_whitespaces_from_list/1,
  remove_whitespaces/1,
  remove_whitespaces_deeply/1
]).

% Misc. functions on the whole XML tree.
-export([
  get_path/2,
  xmlel_to_xmlelement/1,
  xmlel_to_xmlelement/3,
  xmlelement_to_xmlel/1,
  xmlelement_to_xmlel/3,
  xmlelement_to_xmlel_and_ns_tables/3,
  node_to_list/3,
  document_to_list/1,
  node_to_binary/3,
  document_to_binary/1,
  node_to_iolist/3,
  document_to_iolist/1,
  deindent_document/1,
  indent_document/2,
  indent_document/3,
  clear_endtag_tuples/1,
  escape_using_entities/1,
  escape_using_cdata/1,
  internal_escaping_function_name/0
]).

% gen_server(3erl) callbacks.
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  engines
}).

-record(xml_engine, {
  name,
  driver_path,
  driver
}).

-record(xml_parser, {
  options = [],
  port
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_ENGINE, expat).

-define(COMMAND_SET_NSPARSER,     1).
-define(COMMAND_SET_NAMEASATOM,   2).
-define(COMMAND_SET_CHECK_NS,     3).
-define(COMMAND_SET_CHECK_NAMES,  4).
-define(COMMAND_SET_CHECK_ATTRS,  5).
-define(COMMAND_SET_MAXSIZE,      6).
-define(COMMAND_SET_ROOTDEPTH,    7).
-define(COMMAND_SET_ENDTAG,       8).
-define(COMMAND_ADD_KNOWN_NS,     9).
-define(COMMAND_ADD_KNOWN_NAME,  10).
-define(COMMAND_ADD_KNOWN_ATTR,  11).
-define(COMMAND_PARSE,           12).
-define(COMMAND_PARSE_FINAL,     13).
-define(COMMAND_SVN_REVISION,    14).

-define(DEFAULT_PARSER_OPTIONS, [
  {namespace, true},
  {name_as_atom, true},
  {ns_check, true},
  {names_check, true},
  {attrs_check, true},
  {endtag, false},
  {root_depth, 0},
  {maxsize, infinity}
]).

-define(PREFIXED_NAME(P, N), P ++ ":" ++ N).

-ifdef(ESCAPE_USING_CDATA_SECTIONS).
-define(ESCAPE(CData), escape_using_cdata(CData)).
-else.
-define(ESCAPE(CData), escape_using_entities(CData)).
-endif.

-define(IMPLICIT_PREFIXED_NS, [
  {?NS_XML, ?NS_XML_pfx}
]).

% --------------------------------------------------------------------
% Initialization.
% --------------------------------------------------------------------

%% @hidden

start() ->
    Ret = gen_server:start({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.

%% @hidden

start_link() ->
    Ret = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.

-ifdef(HAVE_EXPAT).
-define(REGISTER_EXPAT,
  register_builtin_engine(expat, exmpp_xml_expat)).
-else.
-define(REGISTER_EXPAT, ok).
-endif.

-ifdef(HAVE_LIBXML2).
-define(REGISTER_LIBXML2,
  register_builtin_engine(libxml2, exmpp_xml_libxml2)).
-else.
-define(REGISTER_LIBXML2, ok).
-endif.

register_builtin_engines() ->
    ?REGISTER_EXPAT,
    ?REGISTER_LIBXML2,
    ok.

register_builtin_engine(Name, Driver) ->
    try
        register_engine(Name, Driver)
    catch
        throw:{port_driver, load, Reason, Driver_Name} ->
            error_logger:warning_msg("Failed to load driver \"~s\": ~s~n",
              [Driver_Name, erl_ddll:format_error(Reason)])
    end.

% --------------------------------------------------------------------
% Registry handling.
% --------------------------------------------------------------------

%% @spec (Name, Driver) -> ok
%%     Name = atom()
%%     Driver = atom()
%% @doc Add a new XML engine.

register_engine(Name, Driver) ->
    register_engine(Name, undefined, Driver).

%% @spec (Name, Driver_Path, Driver) -> ok
%%     Name = atom()
%%     Driver_Path = string()
%%     Driver = atom()
%% @doc Add a new XML engine.

register_engine(Name, Driver_Path, Driver)
  when is_atom(Name) ->
    Engine = #xml_engine{
      name = Name,
      driver_path = Driver_Path,
      driver = Driver
    },
    case gen_server:call(?SERVER, {register_engine, Engine}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of XML engines.

get_engine_names() ->
    gen_server:call(?SERVER, get_engine_names, infinity).

%% @spec (Engine_Name) -> bool()
%%     Engine_Name = atom()
%% @doc Tell if `Engine_Name' is available.

is_engine_available(Engine_Name) ->
    case gen_server:call(?SERVER, {get_engine, Engine_Name}, infinity) of
        undefined -> false;
        _         -> true
    end.

%% @spec (Engine_Name) -> Driver_Name
%%     Engine_Name = atom()
%%     Driver_Name = atom()
%% @doc Return the port driver name associated to the given engine.

get_engine_driver(Engine_Name) ->
    case gen_server:call(?SERVER, {get_engine, Engine_Name}, infinity) of
        undefined                         -> undefined;
        #xml_engine{driver = Driver_Name} -> Driver_Name
    end.

% --------------------------------------------------------------------
% Parsing functions (interface to the Expat port driver).
% --------------------------------------------------------------------

%% @spec () -> Parser
%%     Parser = xmlparser()
%% @doc Initialize the Expat port driver with default options.
%%
%% Default options are:
%% ```
%% [namespace, name_as_atom, {endtag, false}, {root_depth, 0}, {maxsize, infinity}]
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
    % Start a port driver instance.
    Driver_Name = get_engine_from_options(Options),
    Port = exmpp_internals:open_port(Driver_Name),

    % Initialize port.
    try
        % Check options.
        Parser = #xml_parser{port = Port},
        New_Options = merge_options(?DEFAULT_PARSER_OPTIONS, Options),
        reset_parser2(Parser, New_Options)
    catch
        _:Exception ->
            exmpp_internals:close_port(Port),
            throw(Exception)
    end.

%% @spec (Parser) -> New_Parser
%%     Parser = xmlparser()
%% @doc Reset the parser with the same previous options.

reset_parser(Parser) ->
    reset_parser(Parser, []).

%% @spec (Parser, Options) -> New_Parser
%%     Parser = xmlparser()
%%     Options = [xmlparseroption()]
%% @doc Reset the parser and update its options.

reset_parser(Parser, Options) ->
    New_Options = merge_options(Parser#xml_parser.options, Options),
    reset_parser2(Parser, New_Options).

reset_parser2(Parser, Options) ->
    case handle_options(Parser, Options) of
        {error, Reason, Infos} ->
            throw({xml_parser, options, Reason, Infos});
        New_Parser ->
            % We can now autoload known namespaces/names/attrs.
            case proplists:get_bool(autoload_known, Options) of
                true  -> autoload_known(New_Parser);
                false -> ok
            end,
            New_Parser
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
add_known_ns(#xml_parser{port = Port} = _Parser, NS) when is_list(NS) ->
    binary_to_term(port_control(Port, ?COMMAND_ADD_KNOWN_NS,
      term_to_binary(NS))).

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
add_known_name(#xml_parser{port = Port} = _Parser, Name) when is_list(Name) ->
    binary_to_term(port_control(Port, ?COMMAND_ADD_KNOWN_NAME,
      term_to_binary(Name))).

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
add_known_attr(#xml_parser{port = Port} = _Parser, Attr) when is_list(Attr) ->
    binary_to_term(port_control(Port, ?COMMAND_ADD_KNOWN_ATTR,
      term_to_binary(Attr))).

%% @spec (Parser) -> ok
%%     Parser = xmlparser()
%% @doc Autoload known namespaces/names/attributes.

autoload_known(Parser) ->
    case application:get_env(exmpp, xml_known_nss) of
        undefined ->
            ok;
        {ok, NSs} when is_list(NSs) ->
            add_known_nss(Parser, NSs);
        {ok, Bad_NSs} ->
            throw({xml, autoload_known, invalid_known_nss, Bad_NSs})
    end,
    case application:get_env(exmpp, xml_known_names) of
        undefined ->
            ok;
        {ok, Names} when is_list(Names) ->
            add_known_names(Parser, Names);
        {ok, Bad_Names} ->
            throw({xml, autoload_known, invalid_known_names, Bad_Names})
    end,
    case application:get_env(exmpp, xml_known_attrs) of
        undefined ->
            ok;
        {ok, Attrs} when is_list(Attrs) ->
            add_known_attrs(Parser, Attrs);
        {ok, Bad_Attrs} ->
            throw({xml, autoload_known, invalid_known_attrs, Bad_Attrs})
    end.

%% @spec (NSs) -> ok
%%     NSS = [NS]
%%     NS = atom() | string()
%% @throws {xml, autoload_known, invalid_known_nss, term()}
%% @doc Add the given NS list to the autoloaded NS list.

add_autoload_known_nss(NSs) when is_list(NSs) ->
    case application:get_env(exmpp, xml_known_nss) of
        undefined ->
            application:set_env(exmpp, xml_known_nss, NSs);
        {ok, Prev_NSs} when is_list(Prev_NSs) ->
            application:set_env(exmpp, xml_known_nss, Prev_NSs ++ NSs);
        {ok, Bad_NSs} ->
            throw({xml, autoload_known, invalid_known_nss, Bad_NSs})
    end.

%% @spec (Names) -> ok
%%     Names = [Name]
%%     Name = atom() | string()
%% @throws {xml, autoload_known, invalid_known_names, term()}
%% @doc Add the given names list to the autoloaded names list.

add_autoload_known_names(Names) when is_list(Names) ->
    case application:get_env(exmpp, xml_known_names) of
        undefined ->
            application:set_env(exmpp, xml_known_names, Names);
        {ok, Prev_Names} when is_list(Prev_Names) ->
            application:set_env(exmpp, xml_known_names, Prev_Names ++ Names);
        {ok, Bad_Names} ->
            throw({xml, autoload_known, invalid_known_names, Bad_Names})
    end.

%% @spec (Attrs) -> ok
%%     Attrs = [Attr]
%%     Attr = atom() | string()
%% @throws {xml, autoload_known, invalid_known_attrs, term()}
%% @doc Add the given attributes list to the autoloaded attributes list.

add_autoload_known_attrs(Attrs) when is_list(Attrs) ->
    case application:get_env(exmpp, xml_known_attrs) of
        undefined ->
            application:set_env(exmpp, xml_known_attrs, Attrs);
        {ok, Prev_Attrs} when is_list(Prev_Attrs) ->
            application:set_env(exmpp, xml_known_attrs, Prev_Attrs ++ Attrs);
        {ok, Bad_Attrs} ->
            throw({xml, autoload_known, invalid_known_attrs, Bad_Attrs})
    end.

%% @spec (Parser, Data) -> [XML_Element] | continue
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlelement() | xmlel() | xmlendtag()
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
    case binary_to_term(port_control(Port, ?COMMAND_PARSE, Data)) of
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
%%     XML_Element = xmlelement() | xmlel() | xmlendtag()
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
    case binary_to_term(port_control(Port, ?COMMAND_PARSE_FINAL, Data)) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            throw({xml_parser, parsing, Reason, undefined});
        {xmlerror, Reason} ->
            throw({xml_parser, parsing, malformed_xml, Reason})
    end.

%% @spec (Document) -> [XML_Element] | done
%%     Document = string() | binary()
%%     XML_Element = xmlel() | xmlelement() | xmlendtag()
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
%%     XML_Element = xmlel() | xmlelement() | xmlendtag()
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
%%     XML_Element = xmlel() | xmlelement() | xmlendtag()
%% @doc Parse a fragment of an XML document at once.
%%
%% This function is useful if you do not have a complete and valid XML
%% document. For instance, something like this:
%% ```
%% <element>content</elem
%% '''
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options, but
%% will set `{root_depth, none}' (which can be overriden); see {@link
%% start_parser/1} for any related informations.

parse_document_fragment(Fragment) ->
    parse_document_fragment(Fragment, []).

%% @spec (Fragment, Parser_Options) -> [XML_Element]
%%     Fragment = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlel() | xmlelement() | xmlendtag()
%% @doc Parse a fragment of an XML document at once.
%%
%% This function is useful if you do not have a complete and valid XML
%% document. For instance, something like this:
%% ```
%% <element>content</elem
%% '''
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser but `{root_depth, none}' is prepended (this can be overriden);
%% see {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

parse_document_fragment(Fragment, Parser_Options) ->
    Parser = start_parser([{root_depth, none} | Parser_Options]),
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
        ?COMMAND_SVN_REVISION, <<>>)),
    Result.

% --------------------------------------------------------------------
% Functions to handle namespaces in XML elements and attributes.
% --------------------------------------------------------------------

%% @spec (XML_Element, NS) -> bool()
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%% @doc Tell if `NS' was declared within this element.

is_ns_declared_here(#xmlel{declared_ns = Declared_NS}, NS) ->
    lists:keymember(NS, 1, Declared_NS).

%% @spec (XML_Element, NS, Prefix) -> New_XML_Element
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%%     Prefix = string()
%%     New_XML_Element = xmlel()
%% @doc Declare the given namespace in this element.

declare_ns_here(#xmlel{declared_ns = Declared_NS} = XML_Element,
  NS, Prefix) ->
    New_Declared_NS = exmpp_utils:keystore(NS, 1,
      Declared_NS, {NS, Prefix}),
    XML_Element#xmlel{declared_ns = New_Declared_NS}.

%% @spec (XML_Element) -> NS
%%     XML_Element = xmlel()
%%     NS = string()
%% @doc Return the namespace as a string, regardless of the original
%% encoding.

get_ns_as_list(#xmlel{ns = undefined}) ->
    "";
get_ns_as_list(#xmlel{ns = NS}) ->
    as_list(NS).

as_list(V) when is_atom(V) -> atom_to_list(V);
as_list(V) when is_list(V) -> V.

%% @spec (XML_Element) -> NS
%%     XML_Element = xmlel()
%%     NS = atom()
%% @doc Return the namespace as an atom, regardless of the original
%% encoding.

get_ns_as_atom(#xmlel{ns = undefined}) ->
    undefined;
get_ns_as_atom(#xmlel{ns = NS}) ->
    as_atom(NS).

as_atom(V) when is_atom(V) -> V;
as_atom(V) when is_list(V) -> list_to_atom(V).

% --------------------------------------------------------------------
% Functions to handle XML attributes (xmlnsattribute() & xmlattribute()).
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (Attr, Name) -> bool()
%%     Attr = xmlnsattribute() | xmlattribute()
%%     Name = atom() | string()
%% @doc Tell if `Attr' is named `Name'.
%%
%% It takes care of comparison between string and atom.

attribute_matches(#xmlattr{name = Name}, Name) ->
    true;
attribute_matches({Name, _Value}, Name) ->
    true;

attribute_matches(#xmlattr{name = Name_A}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
attribute_matches(#xmlattr{name = Name}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

attribute_matches({Name_A, _Value}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
attribute_matches({Name, _Value}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

attribute_matches(_Attr, _Name) ->
    false.

%% @spec (Attr, NS, Name) -> bool()
%%     Attr = xmlnsattribute()
%%     NS = atom() | string()
%%     Name = atom() | string()
%% @doc Tell if `Attr' has the namespace `NS' and is named `Name'.
%%
%% It takes care of comparison between string and atom.

attribute_matches(#xmlattr{ns = NS, name = Name}, NS, Name) ->
    true;

attribute_matches(#xmlattr{ns = NS_A, name = Name_A}, NS, Name)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);
attribute_matches(#xmlattr{ns = NS, name = Name}, NS_A, Name_A)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);

attribute_matches(#xmlattr{ns = NS_A, name = Name}, NS, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);
attribute_matches(#xmlattr{ns = NS, name = Name}, NS_A, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);

attribute_matches(#xmlattr{ns = NS, name = Name_A}, NS, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
attribute_matches(#xmlattr{ns = NS, name = Name}, NS, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

attribute_matches(_Attr, _NS, _Name) ->
    false.

%% @spec (Name) -> Attr
%%     Name = atom() | string()
%%     Attr = xmlnsattribute()
%% @doc Create an XML attribute with the name `Name'.
%%
%% This is the same as:
%% ```
%% Attr = #xmlattr{name = Name}.
%% '''

make_attribute(Name) ->
    #xmlattr{name = Name}.

%% @spec (NS, Name) -> Attr
%%     NS = atom() | string() | undefined
%%     Name = atom() | string()
%%     Attr = xmlnsattribute()
%% @doc Create an XML attribute with the name `Name' in the namespace `NS'.
%%
%% This is the same as:
%% ```
%% Attr = #xmlattr{ns = NS, name = Name}.
%% '''

make_attribute(NS, Name) ->
    #xmlattr{ns = NS, name = Name}.

%% @spec (Attrs, Attr_Name) -> Attr | undefined
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name' from the list.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node_from_list([Attr | Rest], Name) ->
    case attribute_matches(Attr, Name) of
        true  -> Attr;
        false -> get_attribute_node_from_list(Rest, Name)
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
    case attribute_matches(Attr, NS, Name) of
        true  -> Attr;
        false -> get_attribute_node_from_list(Rest, NS, Name)
    end;
get_attribute_node_from_list([], _NS, _Name) ->
    undefined.

%% @spec (XML_Element, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute() | xmlattribute()
%% @doc Return the attribute named `Attr_Name'.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node(#xmlel{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Attr = xmlnsattribute()
%% @doc Return the attribute named `Attr_Name' with the `NS' namespace URI.
%%
%% Return `undefined' if the attribute isn't found.

get_attribute_node(#xmlel{attrs = Attrs} = _XML_Element, NS, Name) ->
    get_attribute_node_from_list(Attrs, NS, Name);
get_attribute_node(undefined, _NS, _Name) ->
    undefined.

%% @spec (Attrs, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%%     Default = term()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' from the list.
%%
%% Return `Default' if the attribute isn't found.

get_attribute_from_list(Attrs, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        {_Name, Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (Attrs, NS, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlnsattribute()]
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Default = term()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' from the
%% list with the `NS' namespace URI.
%%
%% Return `Default' if the attribute isn't found.

get_attribute_from_list(Attrs, NS, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, NS, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (XML_Element, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%%     Default = term()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name'.
%%
%% Return `Default' if the attribute isn't found.

get_attribute(#xmlel{attrs = Attrs} = _XML_Element, Name, Default) ->
    get_attribute_from_list(Attrs, Name, Default);
get_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name, Default) ->
    get_attribute_from_list(Attrs, Name, Default);
get_attribute(undefined, _Name, Default) ->
    Default.

%% @spec (XML_Element, NS, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Default = term()
%%     Attr_Value = string()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI.
%%
%% Return `Default' if the attribute isn't found.

get_attribute(#xmlel{attrs = Attrs} = _XML_Element, NS, Name, Default) ->
    get_attribute_from_list(Attrs, NS, Name, Default);
get_attribute(undefined, _NS, _Name, Default) ->
    Default.

%% @spec (Attrs, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%% @doc Check the presence for attribute `Attr_Name' in the list.

has_attribute_in_list(Attrs, Name) ->
    case get_attribute_node_from_list(Attrs, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (Attrs, NS, Attr_Name) -> bool()
%%     Attrs = [xmlattribute() | xmlattribute()]
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%% @doc Check the presence for attribute `Attr_Name' with namespace `NS'
%% in the list.

has_attribute_in_list(Attrs, NS, Name) ->
    case get_attribute_node_from_list(Attrs, NS, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, Attr_Name) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Attr_Name = atom() | string()
%% @doc Check the presence for attribute `Attr_Name' in the XML element.

has_attribute(#xmlel{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(undefined, _Name) ->
    false.

%% @spec (XML_Element, NS, Attr_Name) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%% @doc Check the presence for attribute `Attr_Name' with namespace `NS'
%% in the XML element.

has_attribute(#xmlel{attrs = Attrs} = _XML_Element, NS, Name) ->
    has_attribute_in_list(Attrs, NS, Name);
has_attribute(undefined, _NS, _Name) ->
    false.

%% @spec (Attrs, Attr) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr = xmlnsattribute() | xmlattribute()
%%     Attr_Name = atom() | string()
%%     Attr_Value = string() | atom() | integer()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If a match is found, `Attr' will replace the old attribute as is,
%% regardless of the format of the latter.

set_attribute_in_list(Attrs, {Name, Value}) ->
    set_attribute_in_list(Attrs, Name, Value);
set_attribute_in_list(Attrs, #xmlattr{} = Attr) ->
    set_attribute_in_list2(Attrs, Attr, []).

set_attribute_in_list2([Attr | Rest],
  #xmlattr{ns = undefined, name = Name} = New_Attr, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true ->
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, New_Attr, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([Attr | Rest],
  #xmlattr{ns = NS, name = Name} = New_Attr, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true ->
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, New_Attr, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([], New_Attr, New_Attrs) ->
    New_Attrs ++ [New_Attr].

%% @spec (Attrs, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     Attr_Name = atom() | string()
%%     Attr_Value = string() | atom() | integer()
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlnsattribute()} record if it can't determine the type from the
%% other attributes.

set_attribute_in_list(Attrs, Name, Value) when is_atom(Value) ->
    set_attribute_in_list(Attrs, Name, atom_to_list(Value));
set_attribute_in_list(Attrs, Name, Value) when is_integer(Value) ->
    set_attribute_in_list(Attrs, Name, integer_to_list(Value));

set_attribute_in_list(Attrs, Name, Value) ->
    set_attribute_in_list2(Attrs, Name, Value, []).

set_attribute_in_list2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true when is_record(Attr, xmlattr) ->
            New_Attr = Attr#xmlattr{value = Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        true when is_tuple(Attr), size(Attr) == 2 ->
            New_Attr = {Name, Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
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
%%     Attr_Value = string() | atom() | integer()
%%     New_Attrs = [xmlnsattribute()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlnsattribute()} record.

set_attribute_in_list(Attrs, NS, Name, Value) when is_atom(Value) ->
    set_attribute_in_list(Attrs, NS, Name, atom_to_list(Value));
set_attribute_in_list(Attrs, NS, Name, Value) when is_integer(Value) ->
    set_attribute_in_list(Attrs, NS, Name, integer_to_list(Value));

set_attribute_in_list(Attrs, NS, Name, Value) ->
    set_attribute_in_list2(Attrs, NS, Name, Value, []).

set_attribute_in_list2([Attr | Rest], NS, Name, Value, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true when is_record(Attr, xmlattr) ->
            New_Attr = Attr#xmlattr{value = Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, NS, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([], NS, Name, Value, New_Attrs) ->
    New_Attrs ++ [#xmlattr{ns = NS, name = Name, value = Value}].

%% @spec (XML_Element, Attr) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Attr = xmlnsattribute() | xmlattribute()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If a match is found, `Attr' will replace the old attribute as is,
%% regardless of the format of the latter.

set_attribute(#xmlel{attrs = Attrs} = XML_Element, Attr) ->
    New_Attrs = set_attribute_in_list(Attrs, Attr),
    XML_Element#xmlel{attrs = New_Attrs};
set_attribute(#xmlelement{attrs = Attrs} = XML_Element, Attr) ->
    New_Attrs = set_attribute_in_list(Attrs, Attr),
    XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (XML_Element, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Attr_Name = atom() | string()
%%     Attr_Value = string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Add a new attribute or change the value of an existing attribute.

set_attribute(XML_Element, Name, Value) when is_atom(Value) ->
    set_attribute(XML_Element, Name, atom_to_list(Value));
set_attribute(XML_Element, Name, Value) when is_integer(Value) ->
    set_attribute(XML_Element, Name, integer_to_list(Value));

set_attribute(#xmlel{attrs = Attrs} = XML_Element, Name, Value) ->
    New_Attrs = set_attribute_ns2(Attrs, Name, Value, []),
    XML_Element#xmlel{attrs = New_Attrs};

set_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name, Value) ->
    New_Attrs = set_attribute2(Attrs, Name, Value, []),
    XML_Element#xmlelement{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true when is_record(Attr, xmlattr) ->
            New_Attr = Attr#xmlattr{value = Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_ns2(Rest, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_ns2([], Name, Value, New_Attrs) ->
    New_Attrs ++ [#xmlattr{name = Name, value = Value}].

set_attribute2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true when is_tuple(Attr), size(Attr) == 2 ->
            New_Attr = {Name, Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute2(Rest, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute2([], Name, Value, New_Attrs) ->
    New_Attrs ++ [{Name, Value}].

%% @spec (XML_Element, NS, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     Attr_Value = string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.

set_attribute(XML_Element, NS, Name, Value) when is_atom(Value) ->
    set_attribute(XML_Element, NS, Name, atom_to_list(Value));
set_attribute(XML_Element, NS, Name, Value) when is_integer(Value) ->
    set_attribute(XML_Element, NS, Name, integer_to_list(Value));

set_attribute(#xmlel{attrs = Attrs} = XML_Element, NS, Name, Value) ->
    New_Attrs = set_attribute_ns2(Attrs, NS, Name, Value, []),
    XML_Element#xmlel{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], NS, Name, Value, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true when is_record(Attr, xmlattr) ->
            New_Attr = Attr#xmlattr{value = Value},
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_ns2(Rest, NS, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_ns2([], NS, Name, Value, New_Attrs) ->
    New_Attrs ++ [#xmlattr{ns = NS, name = Name, value = Value}].

%% @spec (XML_Element, Attrs_Spec) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Attrs_Spec = [{Name, Value} | {NS, Name, Value} | xmlattribute() | xmlnsattribute()]
%%       NS = atom() | string()
%%       Name = atom() | string()
%%       Value = string()
%%     New_XML_Element = xmlel() | xmlelement()
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

set_attributes(XML_Element, [#xmlattr{} = Attr | Rest]) ->
    New_XML_Element = set_attribute(XML_Element, Attr),
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
    case attribute_matches(Attr, Name) of
        true when is_record(Attr, xmlattr) ->
            lists:reverse(New_Attrs) ++ Rest;
        true when is_tuple(Attr), size(Attr) == 2 ->
            lists:reverse(New_Attrs) ++ Rest;
        false ->
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
    case attribute_matches(Attr, NS, Name) of
        true when is_record(Attr, xmlattr) ->
            lists:reverse(New_Attrs) ++ Rest;
        false ->
            remove_attribute_from_list2(Rest, NS, Name,
              [Attr | New_Attrs])
    end;
remove_attribute_from_list2([], _NS, _Name, New_Attrs) ->
    lists:reverse(New_Attrs).

%% @spec (XML_Element, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Attr_Name = atom() | string()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove attribute named `Attr_Name' and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute(#xmlel{attrs = Attrs} = XML_Element, Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, Name),
    XML_Element#xmlel{attrs = New_Attrs};

remove_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, Name),
    XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (XML_Element, NS, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%%     Attr_Name = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

remove_attribute(#xmlel{attrs = Attrs} = XML_Element, NS, Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, NS, Name),
    XML_Element#xmlel{attrs = New_Attrs}.

% --------------------------------------------------------------------
% Functions to handle XML elements (xmlel() & xmlelement()).
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------

%% @spec (XML_Element) -> Name
%%     XML_Element = xmlel() | xmlelement()
%%     Name = list()
%% @doc Return the name of an element as list, regardless of the
%% original encoding.

get_name_as_list(#xmlel{name = Name}) ->
    as_list(Name);
get_name_as_list(#xmlelement{name = Name}) ->
    as_list(Name).

%% @spec (XML_Element) -> Name
%%     XML_Element = xmlel() | xmlelement()
%%     Name = atom()
%% @doc Return the name of an element as atom, regardless of the
%% original encoding.

get_name_as_atom(#xmlel{name = Name}) ->
    as_atom(Name);
get_name_as_atom(#xmlelement{name = Name}) ->
    as_atom(Name).

%% @spec (XML_Element, Name) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%% @doc Tell if `XML_Element' is named `Name'.
%%
%% It takes care of comparison between string and atom.

element_matches(#xmlel{name = Name}, Name) ->
    true;
element_matches(#xmlelement{name = Name}, Name) ->
    true;

element_matches(#xmlel{name = Name_A}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlel{name = Name}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(#xmlelement{name = Name_A}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlelement{name = Name}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(_XML_Element, _Name) ->
    false.

%% @spec (XML_Element, NS, Name) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%% @doc Tell if `XML_Element' has the namespace `NS' and is named `Name'.
%%
%% It takes care of comparison between string and atom.

element_matches(#xmlel{ns = NS, name = Name}, NS, Name) ->
    true;

element_matches(#xmlel{ns = NS_A, name = Name_A}, NS, Name)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);
element_matches(#xmlel{ns = NS, name = Name}, NS_A, Name_A)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);

element_matches(#xmlel{ns = NS_A, name = Name}, NS, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);
element_matches(#xmlel{ns = NS, name = Name}, NS_A, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);

element_matches(#xmlel{ns = NS, name = Name_A}, NS, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlel{ns = NS, name = Name}, NS, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(_XML_Element, _NS, _Name) ->
    false.

%% @spec (XML_Element, NS) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     NS = atom() | string()
%% @doc Tell if `XML_Element' has the namespace `NS'.
%%
%% It takes care of comparison between string and atom.

element_matches_by_ns(#xmlel{ns = NS}, NS) ->
    true;

element_matches_by_ns(#xmlel{ns = NS_A}, NS)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);
element_matches_by_ns(#xmlel{ns = NS}, NS_A)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);

element_matches_by_ns(_XML_Element, _NS) ->
    false.

%% @spec (Name) -> XML_Element
%%     Name = atom() | string()
%%     XML_Element = xmlel()
%% @doc Create an XML element with the name `Name' but no namespace.
%%
%% This is the same as:
%% ```
%% XML_Element = #xmlel{name = Name}.
%% '''

make_element(Name) ->
    #xmlel{name = Name}.

%% @spec (NS, Name) -> XML_Element
%%     NS = atom() | string() | undefined
%%     Name = atom() | string()
%%     XML_Element = xmlel()
%% @doc Create an XML element with the name `Name' in the namespace `NS'.
%%
%% This is the same as:
%% ```
%% XML_Element = #xmlel{ns = NS, name = Name}.
%% '''

make_element(NS, Name) ->
    #xmlel{ns = NS, name = Name}.

%% @spec (XML_Element, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlelement()
%% @doc Search in the children of `XML_Element' an element named `Name'.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

get_element(#xmlel{children = Children}, Name) ->
    get_element2(Children, Name);
get_element(#xmlelement{children = Children}, Name) ->
    get_element2(Children, Name);
get_element(undefined, _Name) ->
    undefined.

get_element2([Node | Rest], Name) ->
    case element_matches(Node, Name) of
        true  -> Node;
        false -> get_element2(Rest, Name)
    end;
get_element2([], _Name) ->
    undefined;
get_element2(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlelement()
%% @deprecated Please use {@link get_element/2} instead.
%% @doc Search in the children of `XML_Element' an element named `Name'.

get_element_by_name(XML_Element, Name) ->
    get_element(XML_Element, Name).

%% @spec (XML_Element, NS, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' an element named `Name'
%% with `NS' namespace URI.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

get_element(#xmlel{children = Children}, NS, Name) ->
    get_element2(Children, NS, Name);
get_element(undefined, _NS, _Name) ->
    undefined.

get_element2([Node | Rest], NS, Name) ->
    case element_matches(Node, NS, Name) of
        true  -> Node;
        false -> get_element2(Rest, NS, Name)
    end;
get_element2([], _NS, _Name) ->
    undefined;
get_element2(undefined, _NS, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @deprecated Please use {@link get_element/3} instead.
%% @doc Search in the children of `XML_Element' an element named `Name'
%% with `NS' namespace URI.

get_element_by_name(XML_Element, NS, Name) ->
    get_element(XML_Element, NS, Name).

%% @spec (XML_Element, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlelement()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name'
%%
%% This will only search among direct children.

get_elements(#xmlel{children = Children}, Name) ->
    get_elements2(Children, Name);
get_elements(#xmlelement{children = Children}, Name) ->
    get_elements2(Children, Name);
get_elements(undefined, _Name) ->
    [].

get_elements2(undefined, _Name) ->
    [];
get_elements2([], _Name) ->
    [];
get_elements2(Children, Name) ->
    lists:filter(filter_by_name(Name), Children).

filter_by_name(Searched_Name) ->
    fun(XML_Element) ->
        element_matches(XML_Element, Searched_Name)
    end.

%% @spec (XML_Element, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlelement()
%% @deprecated Please use {@link get_elements/2} instead.
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name'

get_elements_by_name(XML_Element, Name) ->
    get_elements(XML_Element, Name).

%% @spec (XML_Element, NS, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name' with `NS' namespace URI.
%%
%% This will only search among direct children.

get_elements(#xmlel{children = Children}, NS, Name) ->
    get_elements2(Children, NS, Name);
get_elements(undefined, _NS, _Name) ->
    [].

get_elements2(undefined, _NS, _Name) ->
    [];
get_elements2([], _NS, _Name) ->
    [];
get_elements2(Children, NS, Name) ->
    lists:filter(filter_by_name(NS, Name), Children).

filter_by_name(Searched_NS, Searched_Name) ->
    fun(XML_Element) ->
        element_matches(XML_Element, Searched_NS, Searched_Name)
    end.

%% @spec (XML_Element, NS, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @deprecated Please use {@link get_elements_by_name/3} instead.
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name' with `NS' namespace URI.

get_elements_by_name(XML_Element, NS, Name) ->
    get_elements(XML_Element, NS, Name).

%% @spec (XML_Element, NS) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' the first element with
%% `NS' namespace URI.
%%
%% If no element with the given namespace is found, it returns
%% `undefined'. This will only search among direct children.
%%
%% This function is particularly usefull to extract XMPP error codes.

get_element_by_ns(#xmlel{children = Children}, NS) ->
    get_element_by_ns2(Children, NS);
get_element_by_ns(undefined, _NS) ->
    undefined.

get_element_by_ns2([Node | Rest], NS) ->
    case element_matches_by_ns(Node, NS) of
        true  -> Node;
        false -> get_element_by_ns2(Rest, NS)
    end;
get_element_by_ns2([], _NS) ->
    undefined;
get_element_by_ns2(undefined, _NS) ->
    undefined.

%% @spec (XML_Element, Name) -> bool()
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%% @doc Check the presence for element `Name' in the children.

has_element(XML_Element, Name) ->
    case get_element(XML_Element, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, NS, Name) -> bool()
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%% @doc Check the presence for element `Name' with `NS' namespace URI in
%% the children.

has_element(XML_Element, NS, Name) ->
    case get_element(XML_Element, NS, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, NS) -> bool()
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%% @doc Check the presence for any elements with `NS' namespace URI in
%% the children.

has_element_by_ns(XML_Element, NS) ->
    case get_element_by_ns(XML_Element, NS) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element) -> [XML_Subelement]
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     XML_Subelement = xmlel() | xmlelement()
%% @doc Get all the element children of the given element, skipping
%% non-element nodes likes cdata.

get_child_elements(#xmlel{children = Children}) ->
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

is_element(#xmlelement{}) -> true;
is_element(#xmlel{})      -> true;
is_element(_)             -> false.

%% @spec (XML_Element, Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove the first child with the name `Name'.

remove_element(#xmlel{children = Children} = XML_Element, Name) ->
    New_Children = remove_element2(Children, Name),
    XML_Element#xmlel{children = New_Children};
remove_element(#xmlelement{children = Children} = XML_Element, Name) ->
    New_Children = remove_element2(Children, Name),
    XML_Element#xmlelement{children = New_Children}.

remove_element2(undefined, _Name) ->
    undefined;
remove_element2(Children, Name) ->
    remove_element3(Children, Name, []).

remove_element3([El | Rest], Name, Result) ->
    case element_matches(El, Name) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element3(Rest, Name, [El | Result])
    end;
remove_element3([], _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS, Name) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove the first child with the name `Name' in the namespace `NS'.

remove_element(#xmlel{children = Children} = XML_Element, NS, Name) ->
    New_Children = remove_element2(Children, NS, Name),
    XML_Element#xmlel{children = New_Children}.

remove_element2(undefined, _NS, _Name) ->
    undefined;
remove_element2(Children, NS, Name) ->
    remove_element3(Children, NS, Name, []).

remove_element3([El | Rest], NS, Name, Result) ->
    case element_matches(El, NS, Name) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element3(Rest, NS, Name, [El | Result])
    end;
remove_element3([], _NS, _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove the first child in the namespace `NS'.

remove_element_by_ns(#xmlel{children = Children} = XML_Element, NS) ->
    New_Children = remove_element_by_ns2(Children, NS),
    XML_Element#xmlel{children = New_Children}.

remove_element_by_ns2(undefined, _NS) ->
    undefined;
remove_element_by_ns2(Children, NS) ->
    remove_element_by_ns3(Children, NS, []).

remove_element_by_ns3([El | Rest], NS, Result) ->
    case element_matches_by_ns(El, NS) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element_by_ns3(Rest, NS, [El | Result])
    end;
remove_element_by_ns3([], _NS, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     Name = atom() | string()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove all children with the name `Name'.

remove_elements(#xmlel{children = Children} = XML_Element, Name) ->
    New_Children = remove_elements2(Children, Name),
    XML_Element#xmlel{children = New_Children};
remove_elements(#xmlelement{children = Children} = XML_Element, Name) ->
    New_Children = remove_elements2(Children, Name),
    XML_Element#xmlelement{children = New_Children}.

remove_elements2(undefined, _Name) ->
    undefined;
remove_elements2(Children, Name) ->
    remove_elements3(Children, Name, []).

remove_elements3([El | Rest], Name, Result) ->
    case element_matches(El, Name) of
        true  -> remove_elements3(Rest, Name, Result);
        false -> remove_elements3(Rest, Name, [El | Result])
    end;
remove_elements3([], _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS, Name) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove all children with the name `Name' in the namespace `NS'.

remove_elements(#xmlel{children = Children} = XML_Element, NS, Name) ->
    New_Children = remove_elements2(Children, NS, Name),
    XML_Element#xmlel{children = New_Children}.

remove_elements2(undefined, _NS, _Name) ->
    undefined;
remove_elements2(Children, NS, Name) ->
    remove_elements3(Children, NS, Name, []).

remove_elements3([El | Rest], NS, Name, Result) ->
    case element_matches(El, NS, Name) of
        true  -> remove_elements3(Rest, NS, Name, Result);
        false -> remove_elements3(Rest, NS, Name, [El | Result])
    end;
remove_elements3([], _NS, _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove all children in the namespace `NS'.

remove_elements_by_ns(#xmlel{children = Children} = XML_Element, NS) ->
    New_Children = remove_elements_by_ns2(Children, NS),
    XML_Element#xmlel{children = New_Children}.

remove_elements_by_ns2(undefined, _NS) ->
    undefined;
remove_elements_by_ns2(Children, NS) ->
    remove_elements_by_ns3(Children, NS, []).

remove_elements_by_ns3([El | Rest], NS, Result) ->
    case element_matches_by_ns(El, NS) of
        true  -> remove_elements_by_ns3(Rest, NS, Result);
        false -> remove_elements_by_ns3(Rest, NS, [El | Result])
    end;
remove_elements_by_ns3([], _NS, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Child = xmlel() | xmlelement() | xmlcdata()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Prepend `Child' to `XML_Element''s children list.

prepend_child(#xmlel{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlel{children = New_Children};
prepend_child(#xmlelement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlelement{children = New_Children};
prepend_child(#xmlel{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlel{children = New_Children};
prepend_child(#xmlelement{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Children = [xmlel() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Prepend every `Children' to `XML_Element''s children list.

prepend_children(#xmlel{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlel{children = New_Children};
prepend_children(#xmlelement{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlelement{children = New_Children};
prepend_children(#xmlel{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlel{children = Concat_Children};
prepend_children(#xmlelement{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlelement{children = Concat_Children}.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Child = xmlel() | xmlelement() | xmlcdata()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Append `Child' to `XML_Element''s children list.

append_child(#xmlel{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlel{children = New_Children};
append_child(#xmlelement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlelement{children = New_Children};
append_child(#xmlel{children = Children} = XML_Element, Child) ->
    New_Children = Children ++ [Child],
    XML_Element#xmlel{children = New_Children};
append_child(#xmlelement{children = Children} = XML_Element, Child) ->
    New_Children = Children ++ [Child],
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Children = [xmlel() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Append every `Children' to `XML_Element''s children list.

append_children(#xmlel{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlel{children = New_Children};
append_children(#xmlelement{children = undefined} = XML_Element,
  New_Children) ->
    XML_Element#xmlelement{children = New_Children};
append_children(#xmlel{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = Children ++ New_Children,
    XML_Element#xmlel{children = Concat_Children};
append_children(#xmlelement{children = Children} = XML_Element,
  New_Children) ->
    Concat_Children = Children ++ New_Children,
    XML_Element#xmlelement{children = Concat_Children}.

%% @spec (XML_Element, Old_Child, New_Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Old_Child = xmlel() | xmlelement()
%%     New_Child = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Replace `Old_Child' by `New_Child' in `XML_Element' children
%% list.

replace_child(#xmlel{children = Children} = XML_Element,
  Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlel{children = New_Children};
replace_child(#xmlelement{children = Children} = XML_Element,
  Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlelement{children = New_Children}.

replace_child2(undefined, _Old_Child, _New_Child) ->
    undefined;
replace_child2([], _Old_Child, _New_Child) ->
    [];
replace_child2(Children, Old_Child, New_Child) ->
    [
      case C of
          Old_Child -> New_Child;
          _         -> C
      end || C <- Children
    ].

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Children = [xmlel() | xmlelement() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Set `XML_Element''s children list to `Children'.
%%
%% Any existing child is removed.

set_children(#xmlel{} = XML_Element, New_Children) ->
    XML_Element#xmlel{children = New_Children};
set_children(#xmlelement{} = XML_Element, New_Children) ->
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Pred, XML_Element) -> New_XML_Element
%%     Pred = function()
%%     Child = xmlel() | xmlelement()
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove any children for which `Pred(Child)' doesn't return `true'.
%%
%% `Pred' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> bool()
%% '''
%%
%% If `children' is `undefined', the function isn't called.

filter(Pred, #xmlel{children = Children} = XML_Element)
  when is_function(Pred, 2) ->
    New_Children = filter2(Pred, XML_Element, Children),
    XML_Element#xmlel{children = New_Children};
filter(Pred, #xmlelement{children = Children} = XML_Element)
  when is_function(Pred, 2) ->
    New_Children = filter2(Pred, XML_Element, Children),
    XML_Element#xmlelement{children = New_Children}.

filter2(_Pred, _XML_Element, undefined) ->
    undefined;
filter2(Pred, XML_Element, Children) ->
    [C || C <- Children, Pred(XML_Element, C)].

%% @spec (Fun, Acc0, XML_Element) -> Acc1
%%     Fun = function()
%%     Acc_In = term()
%%     Child = xmlel() | xmlelement() | undefined
%%     Acc_Out = term()
%%     Acc0 = term()
%%     XML_Element = xmlel() | xmlelement()
%%     Acc1 = term()
%% @doc Call `Fun' for each `XML_Element''s children and return the last
%% accumulator.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(Acc_In, XML_Element, Child) -> Acc_Out
%% '''

fold(Fun, Acc0, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 3) ->
    fold2(Fun, Acc0, XML_Element, Children);
fold(Fun, Acc0, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 3) ->
    fold2(Fun, Acc0, XML_Element, Children).

fold2(Fun, Acc_In, XML_Element, undefined) ->
    Fun(Acc_In, XML_Element, undefined);
fold2(Fun, Acc_In, XML_Element, [Child | Rest]) ->
    fold2(Fun, Fun(Acc_In, XML_Element, Child), XML_Element, Rest);
fold2(_Fun, Acc_Out, _XML_Element, []) ->
    Acc_Out.

%% @spec (Fun, XML_Element) -> ok
%%     Fun = function()
%%     Child = xmlel() | xmlelement() | undefined
%%     XML_Element = xmlel() | xmlelement()
%% @doc Call `Fun' for each `XML_Element''s children.
%%
%% `Fun' return value is ignored.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> Acc_Out
%% '''

foreach(Fun, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    foreach2(Fun, XML_Element, Children);
foreach(Fun, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    foreach2(Fun, XML_Element, Children).

foreach2(Fun, XML_Element, undefined) ->
    Fun(XML_Element, undefined);
foreach2(Fun, XML_Element, [Child | Rest]) ->
    Fun(XML_Element, Child),
    foreach2(Fun, XML_Element, Rest);
foreach2(_Fun, _XML_Element, []) ->
    ok.

%% @spec(Fun, XML_Element) -> New_XML_Element
%%     Fun = function()
%%     Child = xmlel() | xmlelement()
%%     New_Child = xmlel() | xmlelement()
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Apply `Fun' on each child and replace the original one with the
%% function return value.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> New_Child
%% '''
%%
%% If `children' is `undefined', the function isn't called.

map(Fun, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    New_Children = map2(Fun, XML_Element, Children),
    XML_Element#xmlel{children = New_Children};
map(Fun, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    New_Children = map2(Fun, XML_Element, Children),
    XML_Element#xmlelement{children = New_Children}.

map2(_Fun, _XML_Element, undefined) ->
    undefined;
map2(Fun, XML_Element, [Child | Rest]) ->
    [Fun(XML_Element, Child) | map2(Fun, XML_Element, Rest)];
map2(_Fun, _XML_Element, []) ->
    [].

% --------------------------------------------------------------------
% Functions to handle XML text nodes.
% This is similar to the DOM interface but NOT compliant.
% --------------------------------------------------------------------


%% @spec (Value) -> CData
%%     Value = binary() | string() | atom() | integer()
%%     CData = xmlcdata()
%% @doc Create a CData node from a value.
cdata(CData) when is_atom(CData) ->
    cdata(atom_to_list(CData));
cdata(CData) when is_integer(CData) ->
    cdata(integer_to_list(CData));
cdata(CData) when is_list(CData) ->
    cdata(list_to_binary(CData));
cdata(CData) when is_binary(CData) ->
    #xmlcdata{cdata = CData}.


%% @spec (Children) -> CData
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
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
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%%     CData = string()
%% @doc Concatenate and return any character data from the given
%% children list.

get_cdata_from_list_as_list(Children) ->
    binary_to_list(get_cdata_from_list(Children)).

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     CData = binary()
%% @doc Concatenate and return any character data of the given XML
%% element.
%%
%% This function is `get_tag_cdata/1' renamed in `get_cdata/1'. It
%% doesn't take a list of children like the old `get_cdata/1', use
%% {@link get_cdata_from_list/1} for this purpose!

get_cdata(#xmlel{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(#xmlelement{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(undefined) ->
    % This clause makes it possible to write code like:
    % exmpp_xml:get_cdata(exmpp_xml:get_element(XML_El, body))
    <<>>.

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlel() | xmlelement() | undefined
%%     CData = string()
%% @doc Concatenate and return any character data of the given XML
%% element.

get_cdata_as_list(XML_Element) ->
    binary_to_list(get_cdata(XML_Element)).

%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
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
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one and remove empty ones.
%%
%% One caveats is the reconstructed {@link xmlcdata()} is appended at
%% the end of the children list.

normalize_cdata(#xmlel{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlel{children = New_Children};
normalize_cdata(#xmlelement{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Children, CData) -> New_Children
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%%     CData = binary() | string() | atom() | integer()
%%     New_Children = [xmlel() | xmlelement() | xmlcdata()]
%% @doc Replace any character data by `CData' in the list.
%%
%% The new `CData' is placed at the end of the children list.

set_cdata_in_list(Children, CData) when is_atom(CData) ->
    set_cdata_in_list(Children, atom_to_list(CData));
set_cdata_in_list(Children, CData) when is_integer(CData) ->
    set_cdata_in_list(Children, integer_to_list(CData));

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
%%     XML_Element = xmlel() | xmlelement()
%%     CData = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Replace any character data by `CData'.
%%
%% The new `CData' is placed at the end of the children list.

set_cdata(#xmlel{children = Children} = XML_Element, CData) ->
    New_Children = set_cdata_in_list(Children, CData),
    XML_Element#xmlel{children = New_Children};
set_cdata(#xmlelement{children = Children} = XML_Element, CData) ->
    New_Children = set_cdata_in_list(Children, CData),
    XML_Element#xmlelement{children = New_Children}.


%% @spec (XML_Element, CData) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     CData = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Append `Child' to `XML_Element''s children list.
append_cdata(Children, CData) ->
    append_child(Children, cdata(CData)).


%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlelement()] | undefined
%% @doc Remove any character data from the given XML element children
%% list.

remove_cdata_from_list(undefined) ->
    undefined;
remove_cdata_from_list(Children) ->
    [Child || Child <- Children, remove_cdata_from_list2(Child)].

remove_cdata_from_list2(#xmlcdata{}) -> false;
remove_cdata_from_list2(_)           -> true.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove any character data from the given XML element.
%%
%% This function doesn't take a list of children like the old
%% `remove_cdata/1', use {@link remove_cdata_from_list/1} for this
%% purpose!

remove_cdata(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_cdata_from_list(Children),
    XML_Element#xmlel{children = New_Children};
remove_cdata(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_cdata_from_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (CData) -> bool()
%%     CData = xmlcdata()
%% @doc Tell if this text node contains only whitespaces.
%%
%% Whitespaces are `\s', `\t', `\n' and `\r'.

is_whitespace(#xmlcdata{cdata = CData}) ->
    is_whitespace2(CData);
is_whitespace(_) ->
    false.

is_whitespace2(<<C:8, Rest/binary>>)
  when C == $\s; C == $\t; C == $\n; C == $\r ->
    is_whitespace2(Rest);
is_whitespace2(<<>>) ->
    true;
is_whitespace2(_CData) ->
    false.

%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlelement() | xmlcdata()] | undefined
%% @doc Remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

remove_whitespaces_from_list(undefined) ->
    undefined;
remove_whitespaces_from_list(Children) ->
    [Child || Child <- Children, not is_whitespace(Child)].

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

remove_whitespaces(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_from_list(Children),
    XML_Element#xmlel{children = New_Children};
remove_whitespaces(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_from_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Remove text nodes containing only whitespaces in every elements
%% in the given tree.
%%
%% @see is_whitespace/1.

remove_whitespaces_deeply(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_deeply2(Children),
    XML_Element#xmlel{children = New_Children};
remove_whitespaces_deeply(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_deeply2(Children),
    XML_Element#xmlelement{children = New_Children}.

remove_whitespaces_deeply2(undefined) ->
    undefined;
remove_whitespaces_deeply2(Children) ->
    remove_whitespaces_deeply3(Children, []).

remove_whitespaces_deeply3([El | Rest], Result)
  when is_record(El, xmlel); is_record(El, xmlelement) ->
    New_El = remove_whitespaces_deeply(El),
    remove_whitespaces_deeply3(Rest, [New_El | Result]);
remove_whitespaces_deeply3([#xmlcdata{} = CData | Rest], Result) ->
    case is_whitespace(CData) of
        true  -> remove_whitespaces_deeply3(Rest, Result);
        false -> remove_whitespaces_deeply3(Rest, [CData | Result])
    end;
remove_whitespaces_deeply3([Other | Rest], Result) ->
    remove_whitespaces_deeply3(Rest, [Other | Result]);
remove_whitespaces_deeply3([], Result) ->
    lists:reverse(Result).

% --------------------------------------------------------------------
% Function to walk the tree.
% --------------------------------------------------------------------

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlel() | xmlelement()
%%     Path = [pathcomponent()]
%%     XML_Subelement = xmlel() | xmlelement()
%%     Attr_Value = string()
%%     CData = binary()
%%     Not_Found = nil() | binary() | undefined
%% @throws {xml, path, ending_component_not_at_the_end, Path} |
%%         {xml, path, invalid_component,               Path}
%% @doc Follow the given path and return what's pointed by the last
%% component of it.
%%
%% `Path' is a list of path components. If a component points to an
%% {@link xmlel()} or {@link xmlelement()}, the function will
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
    case get_element(XML_Element, Name) of
        undefined      -> get_path_not_found(Path);
        XML_Subelement -> get_path(XML_Subelement, Path)
    end;
get_path(XML_Element, [{element, NS, Name} | Path]) ->
    case get_element(XML_Element, NS, Name) of
        undefined      -> get_path_not_found(Path);
        XML_Subelement -> get_path(XML_Subelement, Path)
    end;
get_path(XML_Element, [{attribute, Name, Default}]) ->
    get_attribute(XML_Element, Name, Default);
get_path(XML_Element, [{attribute, NS, Name, Default}]) ->
    get_attribute(XML_Element, NS, Name, Default);
get_path(XML_Element, [cdata]) ->
    get_cdata(XML_Element);
get_path(XML_Element, [cdata_as_list]) ->
    get_cdata_as_list(XML_Element);
get_path(XML_Element, []) ->
    XML_Element;
get_path(_XML_Element, [{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [cdata_as_list | _Rest] = Path) ->
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
get_path_not_found([cdata_as_list]) ->
    "";
get_path_not_found([]) ->
    undefined;
get_path_not_found([{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found([cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found([cdata_as_list | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found(Path) ->
    throw({xml, path, invalid_component, Path}).

% --------------------------------------------------------------------
% Converters.
% --------------------------------------------------------------------

%% @spec (XML_NS_Element) -> XML_Element
%%     XML_NS_Element = xmlel() | xmlelement() | xmlcdata()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlel()} to an {@link xmlelement()} tuple.
%%
%% Other tuples are ignored.

xmlel_to_xmlelement(XML_Element) ->
    xmlel_to_xmlelement(XML_Element, [], []).

%% @spec (XML_NS_Element, Default_NS, Prefixed_NS) -> XML_Element
%%     XML_NS_Element = xmlel() | xmlelement() | xmlcdata()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlel()} to an {@link xmlelement()} tuple.
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
%%
%% `Default_NS' may be a list of equivalent namespaces. This is useful
%% when stanzas go to and from streams with compatible but different
%% namespaces. Here is an example with `jabber:client', `jabber:server'
%% and `jabber:component:accept':
%% ```
%% exmpp_stanza:to_list(El,
%%   [?NS_JABBER_CLIENT, ?NS_JABBER_SERVER, ?NS_COMPONENT_ACCEPT]).
%% '''

xmlel_to_xmlelement(#xmlel{children = Children} = El,
  Default_NS, Prefixed_NS) ->
    % Unresolve namespaces.
    {New_Name, New_Attrs, Default_NS1, Prefixed_NS1} = unresolve_xmlel_nss(El,
      Default_NS, Prefixed_NS),
    % Treat children.
    New_Children = xmlels_to_xmlelements(Children, Default_NS1, Prefixed_NS1),
    % Now, create the final #xmlelement.
    #xmlelement{name = New_Name, attrs = New_Attrs, children = New_Children};
xmlel_to_xmlelement(#xmlendtag{} = Endtag,
  Default_NS, Prefixed_NS) ->
    % Unresolve namespaces.
    New_Name = unresolve_endtag_nss(Endtag, Default_NS, Prefixed_NS),
    % Now, recreate the final #xmlendtag.
    #xmlendtag{ns = undefined, prefix = undefined, name = New_Name};
xmlel_to_xmlelement(XML_El, _Default_NS, _Prefixed_NS) ->
    % xmlelement() or xmlcdata().
    XML_El.

unresolve_xmlel_nss(#xmlel{ns = NS, name = Name, attrs = Attrs,
  declared_ns = Declared_NS},
  Default_NS, Prefixed_NS) ->
    % First, we add namespace declarations to element attributes.
    {Prefix, Attrs1, Default_NS1, Prefixed_NS1} = forward_declare_ns(NS,
      lists:reverse(Declared_NS), Attrs, Default_NS, Prefixed_NS),
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
    {New_Name, New_Attrs, Default_NS1, Prefixed_NS2}.

unresolve_endtag_nss(#xmlendtag{ns = NS, name = Name,
  prefix = Wanted_Prefix}, Default_NS, Prefixed_NS) ->
    Name_S = if
        is_atom(Name) -> atom_to_list(Name);
        true          -> Name
    end,
    Use_Default_NS = use_default_ns(NS, Default_NS),
    case Use_Default_NS of
        true ->
            % This end tag uses the default namespace.
            Name_S;
        false ->
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
    end.

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
                      [NS_Decl | Converted_Attrs],
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
xmlels_to_xmlelements(undefined, _Default_NS, _Prefixed_NS) ->
    undefined;
xmlels_to_xmlelements([], _Default_NS, _Prefixed_NS) ->
    [];
xmlels_to_xmlelements(XML_Elements, Default_NS, Prefixed_NS) ->
    xmlels_to_xmlelements2(XML_Elements, [], Default_NS, Prefixed_NS).

xmlels_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
  Default_NS, Prefixed_NS) ->
    XML_Element = xmlel_to_xmlelement(XML_NS_Element,
      Default_NS, Prefixed_NS),
    xmlels_to_xmlelements2(Rest,
      [XML_Element | XML_Elements], Default_NS, Prefixed_NS);
xmlels_to_xmlelements2([], XML_Elements, _Default_NS, _Prefixed_NS) ->
    lists:reverse(XML_Elements).

% Helpers.
use_default_ns(NS, Default_NS) ->
    case Default_NS of
        [NS | _] ->
            true;
        [[X | _] = Default_NS1 | _] when is_atom(X); is_list(X) ->
            lists:member(NS, Default_NS1);
        _ ->
            false
    end.

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

forward_declare_ns(Curr_NS, [{undefined = NS, none} | Rest],
  Attrs, [], Prefixed_NS) ->
    New_Default_NS = [NS],
    forward_declare_ns(Curr_NS, Rest,
      Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(undefined = Curr_NS, Declared_NS,
  Attrs, [], Prefixed_NS) ->
    New_Default_NS = [Curr_NS],
    forward_declare_ns(Curr_NS, Declared_NS,
      Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(Curr_NS, [{NS, none} | Rest],
  Attrs, Default_NS, Prefixed_NS) ->
    % Forward-declare a default namespace.
    NS_S = if
        NS == undefined -> "";
        is_atom(NS)     -> atom_to_list(NS);
        true            -> NS
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
%forward_declare_ns(undefined, [], Attrs, Default_NS, Prefixed_NS) ->
%    {none, Attrs, Default_NS, Prefixed_NS};
forward_declare_ns(Curr_NS, [], Attrs, Default_NS, Prefixed_NS) ->
    % We finish with the current namespace of the element.
    Use_Default_NS = use_default_ns(Curr_NS, Default_NS),
    case Use_Default_NS of
        true ->
            % The element belongs to the current default namespace.
            % There's nothing to do.
            {none, Attrs, Default_NS, Prefixed_NS};
        false ->
            % We look for a prefixed namespace.
            case search_in_prefixed_ns(Curr_NS, Prefixed_NS) of
                undefined ->
                    % This element uses a new namespace: it'll become
                    % the new default one.
                    Curr_NS_S = if
                        Curr_NS == undefined -> "";
                        is_atom(Curr_NS)     -> atom_to_list(Curr_NS);
                        true                 -> Curr_NS
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
%%     XML_NS_Element = xmlel() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.

xmlelement_to_xmlel(XML_Element) ->
    xmlelement_to_xmlel(XML_Element, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_NS_Element
%%     XML_Element = xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlel() | xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlelement()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlel_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.

xmlelement_to_xmlel(XML_El, Default_NS, Prefixed_NS) ->
    {New_XML_El, _, _} = xmlelement_to_xmlel_and_ns_tables(XML_El,
      Default_NS, Prefixed_NS),
    New_XML_El.

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> {XML_NS_Element, New_Default_NS, New_Prefixed_NS}
%%     XML_Element = xmlelement() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlel() | xmlelement() | xmlcdata()
%%     New_Default_NS = [NS]
%%     New_Prefixed_NS = [{NS, Prefix}]
%% @doc Convert an {@link xmlelement()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlel_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.
%%
%% This function will returned updated namespaces tables
%% `New_Default_NS' and `New_Prefixed_NS' which can be used for future
%% calls.

xmlelement_to_xmlel_and_ns_tables(
  #xmlelement{name = Name, attrs = Attrs, children = Children},
  Default_NS, Prefixed_NS) ->
    % Udpate NS tables by looking at each attribute for NS declarations.
    % These later are removed at the same time.
    {Declared_NS, Attrs1, Default_NS1, Prefixed_NS1} =
      update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS),
    % Convert attributes and children to the new format.
    New_Attrs = xmlattributes_to_xmlnsattributes(Attrs1, Prefixed_NS1),
    New_Children = xmlelements_to_xmlels(Children,
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
                    #xmlel{
                      ns = undefined,
                      declared_ns = Declared_NS,
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    };
                NS ->
                    #xmlel{
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
                    #xmlel{
                      ns = NS,
                      declared_ns = lists:delete({NS, none}, Declared_NS),
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    };
                _ ->
                    % No default namespace declared.
                    #xmlel{
                      ns = undefined,
                      declared_ns = Declared_NS,
                      name = Real_Name_A,
                      attrs = New_Attrs,
                      children = New_Children
                    }
            end
    end,
    {XML_NS_Element, Default_NS1, Prefixed_NS1};
xmlelement_to_xmlel_and_ns_tables(
  #xmlendtag{name = Name}, Default_NS, Prefixed_NS) ->
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
                    #xmlendtag{
                      ns = undefined,
                      prefix = Prefix,
                      name = Real_Name_A
                    };
                NS ->
                    #xmlendtag{
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
                    #xmlendtag{
                      ns = NS,
                      prefix = undefined,
                      name = Real_Name_A
                    };
                _ ->
                    % No default namespace declared.
                    #xmlendtag{
                      ns = undefined,
                      prefix = undefined,
                      name = Real_Name_A
                    }
            end
    end,
    {XML_NS_Element, Default_NS, Prefixed_NS};
xmlelement_to_xmlel_and_ns_tables(XML_El, Default_NS, Prefixed_NS) ->
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
              Default_NS, Prefixed_NS, Declared_NS, [Attr | Purged_Attrs])
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
xmlelements_to_xmlels(undefined, _Default_NS, _Prefixed_NS) ->
    undefined;
xmlelements_to_xmlels([], _Default_NS, _Prefixed_NS) ->
    [];
xmlelements_to_xmlels(XML_Elements, Default_NS, Prefixed_NS) ->
    xmlelements_to_xmlels2(XML_Elements, [],
      Default_NS, Prefixed_NS).

xmlelements_to_xmlels2([XML_Element | Rest], XML_NS_Elements,
  Default_NS, Prefixed_NS) ->
    XML_NS_Element = xmlelement_to_xmlel(XML_Element,
      Default_NS, Prefixed_NS),
    xmlelements_to_xmlels2(Rest,
      [XML_NS_Element | XML_NS_Elements], Default_NS, Prefixed_NS);
xmlelements_to_xmlels2([], XML_NS_Elements, _Default_NS, _Prefixed_NS) ->
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
%%     XML_Element = xmlel() | xmlelement() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = string()
%% @doc Serialize an XML node to text.
%%
%% `Default_NS' and `Prefixed_NS' contain namespace declaration which
%% occured above this node in the tree. The order in the first list is
%% important: declarations are sorted from the most recent one to the
%% oldest one.

node_to_list(El, Default_NS, Prefixed_NS) when is_list(El) ->
    lists:append([node_to_list(E, Default_NS, Prefixed_NS) || E <- El]);

node_to_list(El, Default_NS, Prefixed_NS) ->
    case El of
        #xmlel{children = Children} ->
            {Name, Attrs, Default_NS1, Prefixed_NS1} = unresolve_xmlel_nss(El,
              Default_NS, Prefixed_NS),
            element_to_list(Name, Attrs, Children, Default_NS1, Prefixed_NS1);
        #xmlelement{name = Name, attrs = Attrs, children = Children} ->
            element_to_list(Name, Attrs, Children, Default_NS, Prefixed_NS);
        #xmlendtag{ns = undefined, name = Name} ->
            endtag_to_list(Name);
        #xmlendtag{} ->
            Name = unresolve_endtag_nss(El, Default_NS, Prefixed_NS),
            endtag_to_list(Name);
        #xmlpi{target = Target, value = Value} ->
            pi_to_list(Target, Value);
        #xmlcdata{cdata = CData} ->
            binary_to_list(?ESCAPE(CData))
    end.

element_to_list(Name, Attrs, Children, Default_NS, Prefixed_NS)
  when is_atom(Name) ->
    element_to_list(atom_to_list(Name), Attrs, Children,
      Default_NS, Prefixed_NS);
element_to_list(Name, Attrs, undefined, _Default_NS, _Prefixed_NS) ->
    % Children may come later, we don't close the tag.
    lists:append(["<", Name, attrs_to_list(Attrs), ">"]);
element_to_list(Name, Attrs, Children, Default_NS, Prefixed_NS) ->
    Norm = normalize_cdata_in_list(Children),
    case Norm of
	[] ->
	    lists:append(["<", Name, attrs_to_list(Attrs), "/>"]);
	_ ->
	    Content = lists:append(
			[node_to_list(E, Default_NS, Prefixed_NS) || E <- Norm]),
	    lists:append(
	      ["<", Name, attrs_to_list(Attrs), ">", Content, "</", Name, ">"])
    end.

endtag_to_list(Name) when is_atom(Name) ->
    endtag_to_list(atom_to_list(Name));
endtag_to_list(Name) ->
    lists:append(["</", Name, ">"]).

pi_to_list(Target, Value) when is_atom(Target) ->
    pi_to_list(atom_to_list(Target), Value);
pi_to_list(Target, Value) ->
    lists:append(["<?", Target, " ", Value, "?>"]).

attrs_to_list(Attrs) ->
    lists:append([attr_to_list(A) || A <- Attrs]).

attr_to_list({Name, Value}) ->
    lists:append([" ", Name, "=\"", escape_using_entities(Value), "\""]).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlelement() | list()
%%     XML_Text = string()
%% @doc Serialize an XML document to text.

document_to_list(El) ->
    node_to_list(El, [], []).

%% @spec (El, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlel() | xmlelement() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = binary()
%% @doc Serialize an XML node to text.
%%
%% Converting to binary is about 15% to 20% faster than converting to a
%% list.

node_to_binary(El, Default_NS, Prefixed_NS) when is_list(El) ->
    node_to_binary2(El, Default_NS, Prefixed_NS, <<>>);
node_to_binary(El, Default_NS, Prefixed_NS) ->
    node_to_binary2([El], Default_NS, Prefixed_NS, <<>>).

node_to_binary2([El | Rest], Default_NS, Prefixed_NS, Buf) ->
    New_Buf = case El of
        #xmlel{children = Children} ->
            {Name, Attrs, Default_NS1, Prefixed_NS1} = unresolve_xmlel_nss(El,
              Default_NS, Prefixed_NS),
            element_to_binary(Name, Attrs, Children,
              Default_NS1, Prefixed_NS1, Buf);
        #xmlelement{name = Name, attrs = Attrs, children = Children} ->
            element_to_binary(Name, Attrs, Children,
              Default_NS, Prefixed_NS, Buf);
        #xmlendtag{ns = undefined, name = Name} ->
            endtag_to_binary(Name, Buf);
        #xmlendtag{} ->
            Name = unresolve_endtag_nss(El, Default_NS, Prefixed_NS),
            endtag_to_binary(Name, Buf);
        #xmlpi{target = Target, value = Value} ->
            pi_to_binary(Target, Value, Buf);
        #xmlcdata{cdata = CData} ->
            Escaped = ?ESCAPE(CData),
            <<Buf/binary, Escaped/binary>>
    end,
    node_to_binary2(Rest, Default_NS, Prefixed_NS, New_Buf);
node_to_binary2([], _Default_NS, _Prefixed_NS, Buf) ->
    Buf.

element_to_binary(Name, Attrs, Children, Default_NS, Prefixed_NS, Buf)
  when is_atom(Name) ->
    element_to_binary(atom_to_list(Name), Attrs, Children,
      Default_NS, Prefixed_NS, Buf);
element_to_binary(Name, Attrs, undefined, _Default_NS, _Prefixed_NS, Buf) ->
    % Children may come later, we don't close the tag.
    Name_B = list_to_binary(Name),
    New_Buf = attrs_to_binary(Attrs, <<Buf/binary, "<", Name_B/binary>>),
    <<New_Buf/binary, ">">>;
element_to_binary(Name, Attrs, [], _Default_NS, _Prefixed_NS, Buf) ->
    Name_B = list_to_binary(Name),
    New_Buf = attrs_to_binary(Attrs, <<Buf/binary, "<", Name_B/binary>>),
    <<New_Buf/binary, "/>">>;
element_to_binary(Name, Attrs, Children, Default_NS, Prefixed_NS, Buf) ->
    Name_B = list_to_binary(Name),
    New_Buf = attrs_to_binary(Attrs, <<Buf/binary, "<", Name_B/binary>>),
    Norm = normalize_cdata_in_list(Children),
    New_Buf2 = node_to_binary2(Norm, Default_NS, Prefixed_NS,
      <<New_Buf/binary, ">">>),
    <<New_Buf2/binary, "</", Name_B/binary, ">">>.

endtag_to_binary(Name, Buf) when is_atom(Name) ->
    endtag_to_binary(atom_to_list(Name), Buf);
endtag_to_binary(Name, Buf) ->
    Name_B = list_to_binary(Name),
    <<Buf/binary, "</", Name_B/binary, ">">>.

pi_to_binary(Target, Value, Buf) when is_atom(Target) ->
    pi_to_binary(atom_to_list(Target), Value, Buf);
pi_to_binary(Target, Value, Buf) ->
    Target_B = list_to_binary(Target),
    Value_B = list_to_binary(Value),
    <<Buf/binary, "<?", Target_B/binary, " ", Value_B/binary, "?>">>.

attrs_to_binary([{Name, Value} | Rest], Buf) ->
    attrs_to_binary(Rest, attr_to_binary(Name, Value, Buf));
attrs_to_binary([], Buf) ->
    Buf.

attr_to_binary(Name, Value, Buf) when is_atom(Name) ->
    attr_to_binary(atom_to_list(Name), Value, Buf);
attr_to_binary(Name, Value, Buf) ->
    Name_B = list_to_binary(Name),
    Value_B = list_to_binary(escape_using_entities(Value)),
    <<Buf/binary, " ", Name_B/binary, "=\"", Value_B/binary, "\"">>.

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlelement() | list()
%%     XML_Text = binary()
%% @doc Serialize an XML document to text.
%%
%% Converting to binary is about 15% to 20% faster than converting to a
%% list.

document_to_binary(El) ->
    node_to_binary(El, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlel() | xmlelement() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = iolist()
%% @doc Serialize an XML node to text.
%%
%% Converting to iolist is about 40% to 50% faster than converting to a
%% list.

node_to_iolist(El, Default_NS, Prefixed_NS) when is_list(El) ->
    node_to_iolist2(El, Default_NS, Prefixed_NS, []);
node_to_iolist(El, Default_NS, Prefixed_NS) ->
    node_to_iolist2([El], Default_NS, Prefixed_NS, []).

node_to_iolist2([El | Rest], Default_NS, Prefixed_NS, IO_List) ->
    Sub_IO_List = case El of
        #xmlel{children = Children} ->
            {Name, Attrs, Default_NS1, Prefixed_NS1} = unresolve_xmlel_nss(El,
              Default_NS, Prefixed_NS),
            element_to_iolist(Name, Attrs, Children,
              Default_NS1, Prefixed_NS1);
        #xmlelement{name = Name, attrs = Attrs, children = Children} ->
            element_to_iolist(Name, Attrs, Children,
              Default_NS, Prefixed_NS);
        #xmlendtag{ns = undefined, name = Name} ->
            endtag_to_iolist(Name);
        #xmlendtag{} ->
            Name = unresolve_endtag_nss(El, Default_NS, Prefixed_NS),
            endtag_to_iolist(Name);
        #xmlpi{target = Target, value = Value} ->
            pi_to_iolist(Target, Value);
        #xmlcdata{cdata = CData} ->
            ?ESCAPE(CData)
    end,
    node_to_iolist2(Rest, Default_NS, Prefixed_NS, [Sub_IO_List | IO_List]);
node_to_iolist2([], _Default_NS, _Prefixed_NS, IO_List) ->
    lists:reverse(IO_List).

element_to_iolist(Name, Attrs, Children, Default_NS, Prefixed_NS)
  when is_atom(Name) ->
    element_to_iolist(atom_to_list(Name), Attrs, Children,
      Default_NS, Prefixed_NS);
element_to_iolist(Name, Attrs, undefined, _Default_NS, _Prefixed_NS) ->
    % Children may come later, we don't close the tag.
    [$<, Name, attrs_to_iolist(Attrs), $>];
element_to_iolist(Name, Attrs, [], _Default_NS, _Prefixed_NS) ->
    [$<, Name, attrs_to_iolist(Attrs), $/, $>];
element_to_iolist(Name, Attrs, Children, Default_NS, Prefixed_NS) ->
    Content = node_to_iolist(Children, Default_NS, Prefixed_NS),
    [$<, Name, attrs_to_iolist(Attrs), $>, Content, $<, $/, Name, $>].

endtag_to_iolist(Name) when is_atom(Name) ->
    endtag_to_iolist(atom_to_list(Name));
endtag_to_iolist(Name) ->
    [$<, $/, Name, $>].

pi_to_iolist(Target, Value) when is_atom(Target) ->
    pi_to_iolist(atom_to_list(Target), Value);
pi_to_iolist(Target, Value) ->
    [$<, $?, Target, $\s, Value, $?, $>].

attrs_to_iolist(Attrs) ->
    [attr_to_iolist(A) || A <- Attrs].

attr_to_iolist({Name, Value}) ->
    [$\s, Name, $=, $", escape_using_entities(Value), $"].

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlelement() | list()
%%     XML_Text = iolist()
%% @doc Serialize an XML document to text.

document_to_iolist(El) ->
    node_to_iolist(El, [], []).

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Recursively remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

deindent_document(#xmlel{children = Children} = El) ->
    New_Children = deindent_children(remove_whitespaces_from_list(Children)),
    El#xmlel{children = New_Children};
deindent_document(#xmlelement{children = Children} = El) ->
    New_Children = deindent_children(remove_whitespaces_from_list(Children)),
    El#xmlelement{children = New_Children}.

deindent_children(Children) ->
    deindent_children2(Children, []).

deindent_children2([Child | Rest], Result)
  when is_record(Child, xmlel); is_record(Child, xmlelement) ->
    New_Child = deindent_document(Child),
    deindent_children2(Rest, [New_Child | Result]);
deindent_children2([#xmlcdata{cdata = CData} | Rest], Result) ->
    New_Child = #xmlcdata{cdata = exmpp_utils:strip(CData)},
    deindent_children2(Rest, [New_Child | Result]);
deindent_children2([], Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, Indent) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Indent = binary()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Add whitespaces text nodes to indent the document.
%%
%% Indentation of {@link xmlendtag()} isn't supported yet.

indent_document(El, Indent) ->
    indent_document(El, Indent, <<>>).

%% @spec (XML_Element, Indent, Previous_Total) -> New_XML_Element
%%     XML_Element = xmlel() | xmlelement()
%%     Indent = binary()
%%     Previous_Total = binary()
%%     New_XML_Element = xmlel() | xmlelement()
%% @doc Add whitespaces text nodes to indent the document.
%%
%% Indentation of {@link xmlendtag()} isn't supported yet.

indent_document(El, Indent, Previous_Total) ->
    % First, we remove previous indentation.
    New_El = deindent_document(El),
    indent_document2(New_El, Indent, Previous_Total).

indent_document2(#xmlel{children = Children} = El,
  Indent, Previous_Total) ->
    New_Children = indent_children(Children, Indent, Previous_Total),
    El#xmlel{children = New_Children};
indent_document2(#xmlelement{children = Children} = El,
  Indent, Previous_Total) ->
    New_Children = indent_children(Children, Indent, Previous_Total),
    El#xmlelement{children = New_Children}.

indent_children(undefined, _Indent, _Previous_Total) ->
    undefined;
indent_children(Children, Indent, Previous_Total) ->
    New_Previous_Total = list_to_binary([Previous_Total, Indent]),
    Before = #xmlcdata{cdata = list_to_binary([<<"\n">>, New_Previous_Total])},
    End = #xmlcdata{cdata = list_to_binary([<<"\n">>, Previous_Total])},
    indent_children2(Children, Indent, New_Previous_Total, Before, End, []).

indent_children2([], _Indent, _Previous_Total, _Before, _End, []) ->
    [];
indent_children2([#xmlcdata{cdata = CData}], _Indent, _Previous_Total,
  _Before, _End, []) ->
    [#xmlcdata{cdata = exmpp_utils:strip(CData)}];
indent_children2([Child | Rest], Indent, Previous_Total, Before, End, Result)
  when is_record(Child, xmlel); is_record(Child, xmlelement) ->
    New_Child = indent_document2(Child, Indent, Previous_Total),
    New_Result = [New_Child, Before | Result],
    indent_children2(Rest, Indent, Previous_Total, Before, End, New_Result);
indent_children2([#xmlcdata{cdata = CData} | Rest], Indent, Previous_Total,
  Before, End, Result) ->
    New_Child = #xmlcdata{cdata = exmpp_utils:strip(CData)},
    New_Result = [New_Child, Before | Result],
    indent_children2(Rest, Indent, Previous_Total, Before, End, New_Result);
indent_children2([], _Indent, _Previous_Total, _Before, End, Result) ->
    lists:reverse([End | Result]).

%% @spec (XML_Elements) -> Cleaned_XML_Elements
%%     XML_Elements = [xmlel() | xmlelement() | xmlcdata() |
%%         xmlendtag()]
%%     Cleaned_XML_Elements = [xmlel() | xmlelement() | xmlcdata()]
%% @doc Remove any {@link xmlendtag()}
%% from the list of XML elements.
%%
%% This is primarily designed to work on returned value of {@link
%% parse/2} and {@link parse_final/2} when the `no_endtag' parser
%% option (see {@link xmlparseroption()}) wasn't specified at {@link
%% start_parser/1} time.

clear_endtag_tuples(XML_Elements) ->
    clear_endtag_tuples2(XML_Elements, []).

clear_endtag_tuples2([#xmlendtag{} | Rest], Result) ->
    clear_endtag_tuples2(Rest, Result);
clear_endtag_tuples2([XML_Element | Rest], Result) ->
    clear_endtag_tuples2(Rest, [XML_Element | Result]);
clear_endtag_tuples2([], Result) ->
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

% Choose the most appropriate engine.
get_engine_from_options(Options) ->
    Engine_Name = case proplists:get_value(engine, Options) of
        undefined ->
            case get_engine_names() of
                [] ->
                    throw({xml_parser, options, no_engine_available,
                        undefined});
                [Name | _] = Names ->
                    case lists:member(?DEFAULT_ENGINE, Names) of
                        true  -> ?DEFAULT_ENGINE;
                        false -> Name
                    end
            end;
        Name ->
            case is_engine_available(Name) of
                true ->
                    Name;
                false ->
                    throw({xml_parser, options, engine_unavailable, Name})
            end
    end,
    get_engine_driver(Engine_Name).

% Merge options to avoid duplicates and multiple initialization of the
% parser.
merge_options(Options, [{Key, _} = Option | Rest]) ->
    New_Options = exmpp_utils:keystore(Key, 1, Options, Option),
    merge_options(New_Options, Rest);
merge_options(Options, [Option | Rest]) when is_atom(Option) ->
    merge_options(Options, [{Option, true} | Rest]);
merge_options(Options, []) ->
    Options.

% Update parser options.
handle_options(#xml_parser{options = Options, port = Port} = Parser,
  [{Key, _} = Option | Rest]) ->
    case set_option(Port, Option) of
        ok ->
            New_Options = exmpp_utils:keystore(Key, 1, Options, Option),
            New_Parser = Parser#xml_parser{options = New_Options},
            handle_options(New_Parser, Rest);
        Error ->
            Error
    end;
handle_options(_Parser, [Invalid_Option | _Rest]) ->
    {error, invalid, Invalid_Option};
handle_options(Parser, []) ->
    Parser.

set_option(_Port, {engine, Engine_Name}) when is_atom(Engine_Name) ->
    ok;

set_option(Port, {namespace, NS}) when is_boolean(NS) ->
    Ret = port_control(Port, ?COMMAND_SET_NSPARSER, term_to_binary(NS)),
    case binary_to_term(Ret) of
        ok              -> ok;
        {error, Reason} -> {error, init, Reason}
    end;

set_option(Port, {name_as_atom, As_Atom}) when is_boolean(As_Atom) ->
    port_control(Port, ?COMMAND_SET_NAMEASATOM, term_to_binary(As_Atom)),
    ok;

set_option(Port, {ns_check, Check}) when is_boolean(Check) ->
    port_control(Port, ?COMMAND_SET_CHECK_NS, term_to_binary(Check)),
    ok;

set_option(Port, {names_check, Check}) when is_boolean(Check) ->
    port_control(Port, ?COMMAND_SET_CHECK_NAMES, term_to_binary(Check)),
    ok;

set_option(Port, {attrs_check, Check}) when is_boolean(Check) ->
    port_control(Port, ?COMMAND_SET_CHECK_ATTRS, term_to_binary(Check)),
    ok;

set_option(Port, {maxsize, infinity}) ->
    port_control(Port, ?COMMAND_SET_MAXSIZE, term_to_binary(-1)),
    ok;
set_option(Port, {maxsize, Max}) when is_integer(Max), Max >= 0 ->
    port_control(Port, ?COMMAND_SET_MAXSIZE, term_to_binary(Max)),
    ok;

set_option(Port, {root_depth, none}) ->
    port_control(Port, ?COMMAND_SET_ROOTDEPTH, term_to_binary(-1)),
    ok;
set_option(Port, {root_depth, Depth}) when is_integer(Depth), Depth >= 0 ->
    port_control(Port, ?COMMAND_SET_ROOTDEPTH, term_to_binary(Depth)),
    ok;

set_option(Port, {endtag, Endtag}) when is_boolean(Endtag) ->
    port_control(Port, ?COMMAND_SET_ENDTAG, term_to_binary(Endtag)),
    ok;

set_option(_Port, {autoload_known, Autoload}) when is_boolean(Autoload) ->
    ok;

set_option(_Port, Invalid_Option) ->
    {error, invalid, Invalid_Option}.

% --------------------------------------------------------------------
% gen_server(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden

init([]) ->
    Engines = dict:new(),
    {ok, #state{engines = Engines}}.

%% @hidden

handle_call({register_engine,
  #xml_engine{name = Name,
    driver_path = Driver_Path, driver = Driver_Name} = Engine},
  _From,
  #state{engines = Engines} = State) ->
    try
        % Load the driver now.
        case Driver_Path of
            undefined ->
                exmpp_internals:load_driver(Driver_Name);
            _ ->
                exmpp_internals:load_driver(Driver_Name, [Driver_Path])
        end,
        % Add engine to the global list.
        New_Engines = dict:store(Name, Engine, Engines),
        {reply, ok, State#state{
          engines = New_Engines
        }}
    catch
        _:Exception ->
            {reply, {error, Exception}, State}
    end;

handle_call(get_engine_names, _From,
  #state{engines = Engines} = State) ->
    {reply, dict:fetch_keys(Engines), State};

handle_call({get_engine, Engine_Name}, _From,
  #state{engines = Engines} = State) ->
    case dict:is_key(Engine_Name, Engines) of
        true  -> {reply, dict:fetch(Engine_Name, Engines), State};
        false -> {reply, undefined, State}
    end;

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
      "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

%% @hidden

handle_cast(Request, State) ->
    error_logger:info_msg("~p:handle_cast/2:~n- Request: ~p~n"
      "- State: ~p~n", [?MODULE, Request, State]),
    {noreply, State}.

%% @hidden

handle_info(Info, State) ->
    error_logger:info_msg("~p:handle_info/2:~n- Info: ~p~n"
      "- State: ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

%% @hidden

code_change(Old_Vsn, State, Extra) ->
    error_logger:info_msg("~p:code_change/3:~n- Old_Vsn: ~p~n- Extra: ~p~n"
      "- State: ~p~n", [?MODULE, Old_Vsn, Extra, State]),
    {ok, State}.

%% @hidden

terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type xmlparser().
%% Handler for the Expat parser, initialized with a call to {@link
%% start_parser/0}.

%% @type xmlparseroption() = Engine | Namespace_Option | Names_Format | Checks | Stanza_Max_Size | Root_Depth | Send_End_Element | Autoload_Known
%%     Engine = {engine, atom()}
%%     Namespace_Option = {namespace, bool()}
%%     Name_Format = {name_as_atom, bool()}
%%     Checks = NS_Check | Names_Check | Attrs_Check
%%       NS_Check = {ns_check, bool()}
%%       Names_Check = {names_check, bool()}
%%       Attrs_Check = {attrs_check, bool()}
%%     Stanza_Max_Size  = {maxsize, infinity} | {maxsize, Size}
%%     Root_Depth = {root_depth, none} | {root_depth, Depth}
%%     Send_End_Element = {endtag, bool()}
%%     Autoload_Known = {autoload_known, bool()}.
%% Options of the form `{Key, bool()}' can be specified as `Key'. See
%% {@link proplists}.
%%
%% <br/>br/>
%% The `engine' option allows one to choose the engine to use. Available
%% engines list can be retrived with {@link get_engine_names/0}.
%%
%% <br/><br/>
%% The `namespace' option enables or disables the support for namespaces
%% respectively. Tag and attribute namespaces are supported.
%%
%% <br/><br/>
%% The `name_as_atom' option sets if element and attribute names should
%% be encoded as an {@link atom()} or a {@link string()} respectively.
%% "Should" because if names or attributes checks fail, a name will be
%% encoded as a `string()' (see next option).
%%
%% <br/><br/>
%% The `Checks' options enable or disable the control of a namespace,
%% an element name or an attribute name if `name_as_atom' is set. This
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
%% The `root_depth' option specicifies at which level the parser stops
%% to split each node and start to produce trees. For example, if the
%% root depth is 0, the parser will return a unique tree for the whole
%% document. If the root depth is 1, then `<stream>' will produce an
%% element without any children and `<presence>' will produce a tree
%% with all its children. With `{root_depth, none}', no tree will be
%% made, ie, each opening tag will produce an element without any
%% children.
%%
%% <br/><br/>
%% The `endtag' option selects if the parser must produce {@link
%% xmlendtag()} when it encouters an end tag above `root_depth'.
%%
%% <br/><br/>
%% The `autoload_known' option selects if namespaces/names/attributes
%% known tables are filled with the ones from the `exmpp' application
%% environment. The parameters are `xml_known_nss', `xml_known_names'
%% and `xml_known_attrs'. If they're present, they must be set to a list
%% of atoms or strings.

%% @type xmlelement() = {xmlelement, Name, Attrs, Children}
%%     Name = string()
%%     Attrs = [xmlattribute()]
%%     Children = [xmlelement() | xmlcdata()] | undefined.
%% Record representing an XML tag.

%% @type xmlel() = {xmlel, NS, Declared_NS, Name, Attrs, Children}
%%     NS = atom() | string()
%%     Declared_NS = [{NS, Prefix} | {NS, none}]
%%     Prefix = string()
%%     Name = atom() | string()
%%     Attrs = [xmlnsattribute()]
%%     Children = [xmlel() | xmlcdata()] | undefined.
%% Record representing an XML tag when namespace support is enabled.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlattribute() = {Name, Value}
%%     Name = atom() | string()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlnsattribute() = {xmlattr, NS, Prefix, Name, Value}
%%     NS = atom() | string()
%%     Prefix = string() | undefined
%%     Name = atom() | string()
%%     Value = string().
%% Represents an tag attribute.

%% @type xmlendtag() = {xmlendtag, NS, Prefix, Name}
%%     NS = atom() | string()
%%     Prefix = string() | undefined
%%     Name = atom() | string().
%% Record representing an XML end tag when namespace support is
%% enabled, for nodes above the configured `root_depth' (see {@link
%% xmlparseroption()}).

%% @type pathcomponent() = {element, Elem_Name} | {element, NS, Elem_Name} | {attribute, Attr_Name} | {attribute, NS, Attr_Name} | cdata | cdata_as_list
%%     NS = atom() | string()
%%     Elem_Name = atom() | string()
%%     Attr_Name = atom() | string().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.
