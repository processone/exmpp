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
%% Note that <strong>namespace support is very experimental</strong> at
%% this time and thus isn't recommended for production.
%% </p>
%%
%% <p><strong>The API isn't stabilized yet</strong>.
%% What's left to be done:</p>
%% <ul>
%% <li>store an attribute in a record, eg #xmlattr{ns, name, value}</li>
%% <li>polish namespace support. This may involve more changes
%% in the structures (to keep tag prefix or to handle namespaced
%% attributes)</li>
%% <li>add validation for namespace</li>
%% <li>make tag name an atom() and add validation</li>
%% <li>rework API (consistent arguments order, function names with more
%% sense, same functions to work on attributes directly or xmlelement(),
%% etc.)</li>
%% </ul>

-module(exmpp_xml).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([start_parser/0, start_parser/1, stop_parser/1, reload_driver/0]).
-export([parse/2, parse_final/2]).
-export([get_ns/1, xmlnselement_to_xmlelement/1]).
-export([get_subtag/2, add_child/2]).
-export([get_tag_attr/2, get_tag_attr_s/2, get_attr/2, get_attr_s/2]).
-export([remove_attr/2, remove_tag_attr/2]).
-export([replace_attr/3, replace_tag_attr/3]).
-export([get_tag_cdata/1, get_cdata/1]).
-export([remove_cdata/1]).
-export([get_path_s/2]).
-export([clear_endelement_tuples/1]).
-export([element_to_string/1, crypt/1]).

-record(xml_parser, {
	port
}).

-define(DRIVER_NAME, expat_drv).
-define(DRIVER_NAME_S, "expat_drv").

-define(EXPAT_SET_NSPARSER,   1).
-define(EXPAT_SET_MAXSIZE,    2).
-define(EXPAT_SET_ROOTDEPTH,  3).
-define(EXPAT_SET_ENDELEMENT, 4).
-define(EXPAT_PARSE,          5).
-define(EXPAT_PARSE_FINAL,    6).

% --------------------------------------------------------------------
% Parsing functions (interface to the Expat port driver).
% --------------------------------------------------------------------

%% @spec () -> {ok, Parser} | {error, Reason}
%%     Parser = xmlparser()
%% @doc Initialize the Expart port driver with default options.
%%
%% Default options are:
%% ```
%% [nonamespace, noendelement, {rootdepth, 0}, nomaxsize]
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
			Options2 = case lists:member(nonamespace, Options) of
				true ->
					Options;
				false ->
					case lists:member(namespace, Options) of
						true ->
							Options;
						false ->
							[nonamespace | Options]
					end
			end,
			New_Parser = handle_options(Options2, Parser),
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
% Functions to handle the tree, generated by the parser.
% --------------------------------------------------------------------

%% @spec (XML_Element) -> Namespace_URI
%%     XML_Element = xmlnselement()
%%     Namespace_URI = atom()
%% @doc Return the namespace URI of a given element.
%%
%% This obviously doesn't work with an {@link xmlelement()} and must not
%% be called if namespace support is not enabled.
get_ns(#xmlnselement{ns = NS}) ->
	NS.

%% @spec (XML_Element, Name) -> XML_Subelement | false
%%     XML_Element = xmlelement() | xmlnselement()
%%     Name = string()
%%     XML_Subelement = xmlelement() | xmlnselement()
%% @doc Search in the children of `XML_Element' a tag named `Name'.
%%
%% If no tag with the given name is found, it returns `false'.
get_subtag(#xmlelement{children = Children}, Name) ->
	get_subtag2(Children, Name);

get_subtag(#xmlnselement{children = Children}, Name) ->
	get_subtag2(Children, Name).

get_subtag2([Child | Rest], Name) ->
	case Child of
		#xmlelement{name = Name} ->
			Child;
		#xmlnselement{name = Name} ->
			Child;
		_ ->
			get_subtag2(Rest, Name)
	end;
get_subtag2([], _Name) ->
	false.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlelement() | xmlnselement()
%%     Child = xmlelement() | xmlnselement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @doc Add `Child' to `XML_Element''s children.
add_child(#xmlelement{children = Children} = XML_Element, Child) ->
	New_Children = [Child | Children],
	XML_Element#xmlelement{children = New_Children};

add_child(#xmlnselement{children = Children} = XML_Element, Child) ->
	New_Children = [Child | Children],
	XML_Element#xmlnselement{children = New_Children}.

%% @spec (Attr_Name, XML_Element) -> {value, Value} | false
%%     Attr_Name = string()
%%     XML_Element = xmlelement() | xmlnselement()
%% @doc Return the `Attr_Name' attribute value (in a tuple) from the
%% `XML_Element' element.
%%
%% Return `false' if the attribute isn't found.
%%
%% @see get_tag_attr_s/2.
%% @see get_attr/2.
get_tag_attr(Attr_Name, #xmlelement{attrs = Attrs}) ->
	get_attr(Attr_Name, Attrs);

get_tag_attr(Attr_Name, #xmlnselement{attrs = Attrs}) ->
	get_attr(Attr_Name, Attrs).

%% @spec (Attr_Name, XML_Element) -> Value | false
%%     Attr_Name = string()
%%     XML_Element = xmlelement() | xmlnslement()
%% @doc Return the `Attr_Name' attribute value from the `XML_Element'
%% element.
%%
%% Return `false' if the attribute isn't found.
%%
%% @see get_tag_attr/2.
%% @see get_attr_s/2.
get_tag_attr_s(Attr_Name, #xmlelement{attrs = Attrs}) ->
	get_attr_s(Attr_Name, Attrs);

get_tag_attr_s(Attr_Name, #xmlnselement{attrs = Attrs}) ->
	get_attr_s(Attr_Name, Attrs).

%% @spec (Attr_Name, Attrs) -> {value, Value} | false
%%     Attr_Name = string()
%%     Attrs = [xmlattribute()]
%% @doc Return the `Attr_Name' attribute value (in a tuple) from the
%% xmlattribute() list.
%%
%% Return `false' if the attribute isn't found.
%%
%% @see get_tag_attr/2.
%% @see get_attr_s/2.
get_attr(Attr_Name, Attrs) ->
	case lists:keysearch(Attr_Name, 1, Attrs) of
		{value, {_Attr_Name, Value}} -> {value, Value};
		_                            -> false
	end.

%% @spec (Attr_Name, Attrs) -> Value | false
%%     Attr_Name = string()
%%     Attrs = [xmlattribute()]
%% @doc Return the `Attr_Name' attribute value from the xmlattribute()
%% list.
%%
%% Return `false' if the attribute isn't found.
%%
%% @see get_tag_attr_s/2.
%% @see get_attr/2.
get_attr_s(Attr_Name, Attrs) ->
	case lists:keysearch(Attr_Name, 1, Attrs) of
		{value, {_Attr_Name, Value}} -> Value;
		_                            -> ""
	end.

%% @spec (Attr_Name, Attrs) -> New_Attrs
%%     Attr_Name = string()
%%     Attrs = [xmlattribute()]
%%     New_Attrs = [xmlattribute()]
%% @doc Remove an attribute and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).
remove_attr(Attr_Name, Attrs) ->
	lists:keydelete(Attr_Name, 1, Attrs).

%% @spec (Attr_Name, XML_Element) -> New_XML_Element
%%     Attr_Name = string()
%%     XML_Element = xmlelement() | xmlnselement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @doc Remove an attribute and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).
remove_tag_attr(Attr_Name, #xmlelement{attrs = Attrs} = XML_Element) ->
	New_Attrs = remove_attr(Attr_Name, Attrs),
	XML_Element#xmlelement{attrs = New_Attrs};
remove_tag_attr(Attr_Name, #xmlnselement{attrs = Attrs} = XML_Element) ->
	New_Attrs = remove_attr(Attr_Name, Attrs),
	XML_Element#xmlnselement{attrs = New_Attrs}.

%% @spec (Attr_Name, Attr_Value, Attrs) -> New_Attrs
%%     Attr_Name = string()
%%     Attr_Value = string()
%%     Attrs = [xmlattribute()]
%%     New_Attrs = [xmlattribute()]
%% @doc Replace `Attr_Name' value with `Attr_Value'.
replace_attr(Attr_Name, Attr_Value, Attrs) ->
	Attrs1 = lists:keydelete(Attr_Name, 1, Attrs),
	[{Attr_Name, Attr_Value} | Attrs1].

%% @spec (Attr_Name, Value, XML_Element) -> New_XML_Element
%%     Attr_Name = string()
%%     Value = string()
%%     XML_Element = xmlelement() | xmlnselement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @doc Replace the value of `Attr_Name' attribute with `Value' in the
%% given `XML_Element' element.
%%
%% If this attribute doesn't exist, it's added to the list.
replace_tag_attr(Attr_Name, Value, #xmlelement{attrs = Attrs} = XML_El) ->
	New_Attrs = replace_attr(Attr_Name, Value, Attrs),
	XML_El#xmlelement{attrs = New_Attrs};

replace_tag_attr(Attr_Name, Value, #xmlnselement{attrs = Attrs} = XML_El) ->
	New_Attrs = replace_attr(Attr_Name, Value, Attrs),
	XML_El#xmlnselement{attrs = New_Attrs}.

%% @spec ([Children]) -> CData
%%     Children = xmlelement() | xmlnselement()
%%     CData = string()
%% @doc Concatenate and return any character data from th given children list.
%%
%% You should use {@link get_tag_cdata/1} rather than this function.
get_cdata(Children) ->
	% The function list_to_binary/1 will concatenate every
	% binaries in the list returned by get_cdata2/2.
	binary_to_list(list_to_binary(get_cdata2(Children, ""))).

%% @hidden
get_cdata2([#xmlcdata{cdata = Chunk} | Rest], Data) ->
	get_cdata2(Rest, [Data, Chunk]);
get_cdata2([_ | Rest], Data) ->
	get_cdata2(Rest, Data);
get_cdata2([], Data) ->
	Data.

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlelement() | xmlnselement()
%%     CData = string()
%% @doc Concatenate and return any character data of the given XML element.
get_tag_cdata(#xmlelement{children = Children}) ->
	get_cdata(Children);

get_tag_cdata(#xmlnselement{children = Children}) ->
	get_cdata(Children).

%% @spec (Children) -> New_Children
%%     Children = [xmlelement() | xmlnselement() | xmlcdata()]
%%     New_Children = [xmlelement() | xmlnselement()]
%% @doc Remove any character data from the given XML element children list.
remove_cdata(Children) ->
	[Child || Child <- Children, remove_cdata_p(Child)].

%% @hidden
remove_cdata_p({xmlcdata, _CData}) -> false;
remove_cdata_p(_) -> true.

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlelement() | xmlnselement()
%%     Path = [pathcomponent()]
%%     XML_Subelement = xmlelement() | xmlnselement()
%%     Attr_Value = string()
%%     CData = string()
%%     Not_Found = nil()
%% @doc Follow the given path and return what's pointed by the last
%% component of it.
%%
%% `Path' is a list of path components. If a component points to an
%% {@link xmlelement()} of {@link xmlnselement()}, the function will
%% look for this element and will use it as a base for the next path
%% component. If a component points to an attribute, the function will
%% look for this attribute in the current element and return its value
%% (see {@link get_tag_attr_s/2} for the possible return values).
%% If a component asks for character data, the function will return
%% character data for the current element (see {@link get_tag_cdata/1}
%% for possible return values). A path will not be followed further
%% after an attribute or a character data component. If an XML element
%% isn't found while walking through the path, an empty string is
%% returned.
get_path_s(XML_Element, [{elem, Name} | Path]) ->
	case get_subtag(XML_Element, Name) of
		false -> "";
		XML_Subelement -> get_path_s(XML_Subelement, Path)
	end;
get_path_s(XML_Element, [{attr, Name}]) ->
	get_tag_attr_s(Name, XML_Element);
get_path_s(XML_Element, [cdata]) ->
	get_tag_cdata(XML_Element);
get_path_s(XML_Element, []) ->
	XML_Element.

%% @spec (XML_NS_Element) -> XML_Element
%%     XML_NS_Element = xmlnselement() | xmlelement() | xmlcdata()
%%     XML_Element = xmlelement() | xmlcdata()
%% @doc Convert an {@link xmlnselement()} to an {@link xmlelement()} tuple.
%%
%% Other tuples are ignored.
%% Note that turning namespace into `xmlns' attributes is really basic
%% at this time. This function doesn't generate prefixes. In XMPP
%% context, this means that the `<stream>' tag won't have a `stream'
%% prefix and won't declare the default namespace (`jabber:client' or
%% `jabber:server') either. Finaly, any attributes which had a prefix in
%% the original XML stream will produce a non well-formed document.
xmlnselement_to_xmlelement(XML_Element) ->
	xmlnselement_to_xmlelement2(XML_Element, []).

xmlnselement_to_xmlelement2(
    #xmlnselement{ns = NS, name = Name, attrs = Attrs,
    children = Children}, NS_Stack) ->
	case NS_Stack of
		[NS | _] ->
			% Same namespace as parent.
			New_NS_Stack = NS_Stack,
			New_Attrs = Attrs;
		_ ->
			% Different namespace.
			New_NS_Stack = [NS | NS_Stack],
			New_Attrs = case NS of
				undefined ->
					Attrs;
				_         ->
					[{"xmlns", atom_to_list(NS)} | Attrs]
			end
	end,
	New_Children = xmlnselements_to_xmlelements(Children, New_NS_Stack),
	#xmlelement{name = Name, attrs = New_Attrs, children = New_Children};
xmlnselement_to_xmlelement2(XML_El, _NS_Stack) ->
	% xmlelement() or xmlcdata().
	XML_El.

xmlnselements_to_xmlelements([], _NS_Stack) ->
	[];
xmlnselements_to_xmlelements(XML_Elements, NS_Stack) ->
	xmlnselements_to_xmlelements2(XML_Elements, [], NS_Stack).

xmlnselements_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
    NS_Stack) ->
	New_XML_NS_Element = xmlnselement_to_xmlelement2(XML_NS_Element,
	    NS_Stack),
	xmlnselements_to_xmlelements2(Rest,
	    [New_XML_NS_Element | XML_Elements], NS_Stack);
xmlnselements_to_xmlelements2([], XML_Elements, _NS_Stack) ->
	lists:reverse(XML_Elements).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlelement() | xmlnselement()
%%     XML_Text = string()
%% @doc Serialize an XML tree to play text.
element_to_string(El) ->
	case El of
		#xmlelement{name = Name, attrs = Attrs, children = Els} ->
			if
				Els /= [] ->
					[$<, Name, attrs_to_list(Attrs), $>,
					    [element_to_string(E) || E <- Els],
					    $<, $/, Name, $>];
				true ->
					[$<, Name, attrs_to_list(Attrs),
					    $/, $>]
			end;
		#xmlnselement{} ->
			element_to_string(xmlnselement_to_xmlelement(El));
		{xmlcdata, CData} when is_binary(CData) ->
			% We avoid calling crypt/1 directly with the binary()
			% because it'll convert it back to binary().
			crypt(binary_to_list(CData));
		{xmlcdata, CData} ->
			crypt(CData)
	end.

attrs_to_list(Attrs) ->
	[attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
	[$\s, crypt(Name), $=, $', crypt(Value), $'].

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @doc Replace sensible characters with entities.
%%
%% Processed characters are <tt>&amp;</tt>, <tt>&lt;</tt>,
%% <tt>&gt;</tt>, <tt>&quot;</tt>, <tt>&apos;</tt>.
crypt(S) when is_list(S) ->
	[case C of
		$& -> "&amp;";
		$< -> "&lt;";
		$> -> "&gt;";
		$" -> "&quot;";
		$' -> "&apos;";
		_ -> C
		end || C <- S];
crypt(S) when is_binary(S) ->
	list_to_binary(crypt(binary_to_list(S))).

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
handle_options([nonamespace | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_NSPARSER, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([nomaxsize | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{maxsize, infinity} | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(-1)),
	handle_options(Rest, Parser);
handle_options([{maxsize, Max} | Rest], #xml_parser{port = P} = Parser)
    when is_integer(Max), Max >= 0 ->
	port_control(P, ?EXPAT_SET_MAXSIZE, term_to_binary(Max)),
	handle_options(Rest, Parser);

handle_options([{rootdepth, Depth} | Rest], #xml_parser{port = P} = Parser)
    when is_integer(Depth), Depth >= 0 ->
	port_control(P, ?EXPAT_SET_ROOTDEPTH, term_to_binary(Depth)),
	handle_options(Rest, Parser);

handle_options([endelement | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ENDELEMENT, term_to_binary(true)),
	handle_options(Rest, Parser);
handle_options([noendelement | Rest], #xml_parser{port = P} = Parser) ->
	port_control(P, ?EXPAT_SET_ENDELEMENT, term_to_binary(false)),
	handle_options(Rest, Parser);

handle_options([], Parser) ->
	Parser.

%% @spec (XML_Elements) -> Cleaned_XML_Elements
%%     XML_Elements = [xmlelement() | xmlnselement() | xmlcdata() |
%%         xmlendelement() | xmlnsendelement()]
%%     Cleaned_XML_Elements = [xmlelement() | xmlnselement() | xmlcdata()]
%% @doc Remove any {@link xmlendelement()} or {@link xmlnsendelement()}
%% from the list of XML elements.
%%
%% This is primarily designed to work on returned value of {@link
%% parse/2} and {@link parse_final/2} when the `noendelement' parser
%% option (see {@link xmlparseroption()}) wasn't specified at {@link
%% start_parser/1} time.
clear_endelement_tuples(XML_Elements) ->
	clear_endelement_tuples2(XML_Elements, []).

clear_endelement_tuples2([#xmlendelement{} | Rest], Result) ->
	clear_endelement_tuples2(Rest, Result);
clear_endelement_tuples2([#xmlnsendelement{} | Rest], Result) ->
	clear_endelement_tuples2(Rest, Result);
clear_endelement_tuples2([XML_Element | Rest], Result) ->
	clear_endelement_tuples2(Rest, [XML_Element | Result]);
clear_endelement_tuples2([], Result) ->
	lists:reverse(Result).

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type xmlparser().
%% Handler for the Expat parser, initialized with a call to {@link
%% start_parser/0}.

%% @type xmlparseroption() = Namespace_Option | Stanza_Max_Size | Root_Depth | Send_End_Element
%%     Namespace_Option = namespace | nonamespace
%%     Stanza_Max_Size  = nomaxsize | {maxsize, infinity} | {maxsize, Size}
%%     Root_Depth = {rootdepth, Depth}
%%     Send_End_Element = endelement | noendelement.
%% The `namespace' and `nonamespace' flags enable or disable the
%% support for namespaces respectively. Note that the support is very
%% experimental. Attributes namespace prefix is resolved by Expat but
%% not handled by the port driver or this module. This means that the
%% name of the attribute will be of the form `URI|Attr_Name'. XML prefix
%% is an exception because it's processed specificaly and for example,
%% `xml:lang' will keep this name.
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
%% The `rootdepth' option specicifies at which level the parser stop
%% to split each node and start to produce trees. For example, if the
%% root depth is 0, the parser will return a unique tree for the whole
%% document. If the root depth is 1, then `<stream>' will produce an
%% element without any children and `<presence' will produce a tree with
%% all its children.
%%
%% <br/><br/>
%% The `endelement' and `noendelement' select if the parser must
%% produce {@link xmlendelement()} or {@link xmlnsendelement()} when it
%% encouters a closing tag above `root_depth'.

%% @type xmlelement() = {xmlelement, Name, Attrs, Children}
%%     Name = string()
%%     Attrs = [xmlattribute()]
%%     Children = [xmlelement() | xmlcdata()].
%% Record representing an XML tag.

%% @type xmlnselement() = {xmlnselement, NS, Name, Attrs, Children}
%%     NS = atom()
%%     Name = string()
%%     Attrs = [xmlattribute()]
%%     Children = [xmlnselement() | xmlcdata()].
%% Record representing an XML tag when namespace support is enabled.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlattribute() = {Name, Value}
%%     Name = string()
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

%% @type pathcomponent() = {elem, Elem_Name} | {attr, Attr_Name} | cdata
%%     Elem_Name = string()
%%     Attr_Name = string().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.
