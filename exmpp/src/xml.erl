% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides the deprecated API found
%% in ejabberd, on top of {@link exmpp_xml}.

%% @deprecated Please use {@link exmpp_xml}.

-module(xml).
-vsn('$Revision$').

-include("exmpp.hrl").

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
	get_path_s/2,
	element_to_string/1,
	crypt/1
]).

% --------------------------------------------------------------------
% Deprecated API (almost renamed functions).
% --------------------------------------------------------------------

%% @spec (XML_Element, Name) -> XML_Subelement | false
%%     XML_Element = xmlnselement() | xmlelement()
%%     Name = string() | atom()
%%     XML_Subelement = xmlelement() | xmlnselement()
%% @deprecated Please use {@link get_element_by_name/2}.
%% @doc Search in the children of `XML_Element' an element named `Name'.

get_subtag(XML_Element, Name) ->
	exmpp_xml:get_element_by_name(XML_Element, Name).

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
	case exmpp_xml:get_attribute_node_from_list(Attrs, Attr_Name) of
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
%% @deprecated Please use {@link set_attribute_in_list/3}.
%% @doc Replace `Attr_Name' value with `Attr_Value'.

replace_attr(Attr_Name, Attr_Value, Attrs) ->
	exmpp_xml:set_attribute_in_list(Attrs, Attr_Name, Attr_Value).

%% @spec (Attr_Name, Value, XML_Element) -> New_XML_Element
%%     Attr_Name = string() | atom()
%%     Value = string()
%%     XML_Element = xmlelement() | xmlnselement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @deprecated Please use {@link set_attribute/3}.
%% @doc Replace the value of `Attr_Name' attribute with `Value' in the
%% given `XML_Element' element.

replace_tag_attr(Attr_Name, Attr_Value, XML_Element) ->
	exmpp_xml:set_attribute(XML_Element, Attr_Name, Attr_Value).

%% @spec (Attr_Name, Attrs) -> New_Attrs
%%     Attr_Name = string() | atom()
%%     Attrs = [xmlnsattribute() | xmlattribute()]
%%     New_Attrs = [xmlnsattribute() | xmlattribute()]
%% @deprecated Please use {@link remove_attribute_from_list/2}.
%% @doc Remove an attribute and return the new list.

remove_attr(Attr_Name, Attrs) ->
	exmpp_xml:remove_attribute_from_list(Attrs, Attr_Name).

%% @spec (Attr_Name, XML_Element) -> New_XML_Element
%%     Attr_Name = string() | atom()
%%     XML_Element = xmlnselement() | xmlelement()
%%     New_XML_Element = xmlelement() | xmlnselement()
%% @deprecated Please use {@link remove_attribute_from_list/2}.
%% @doc Remove an attribute and return the new element.

remove_tag_attr(Attr_Name, XML_Element) ->
	exmpp_xml:remove_attribute(XML_Element, Attr_Name).

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlnselement() | xmlelement()
%%     CData = string()
%% @deprecated Please use {@link get_cdata/1}.
%% @doc Concatenate and return any character data of the given XML element.

get_tag_cdata(XML_Element) ->
	exmpp_xml:get_cdata(XML_Element).

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlnselement() | xmlelement()
%%     Path = [pathcomponent() | pathcomponentold()]
%%     XML_Subelement = xmlnselement() | xmlelement()
%%     Attr_Value = string()
%%     CData = string()
%%     Not_Found = nil()
%% @deprecated Please use {@link get_path/2}.
%% @doc Follow the given path and return what's pointed by the last
%% component of it.

get_path_s(XML_Element, Path) ->
	New_Path = update_path(Path),
	exmpp_xml:get_path(XML_Element, New_Path).

update_path(Path) ->
	update_path2(Path, []).

update_path2([{elem, Name} | Rest], New_Path) ->
	update_path2(Rest, [{element, Name} | New_Path]);
update_path2([{attr, Name} | Rest], New_Path) ->
	update_path2(Rest, [{attribute, Name} | New_Path]);
update_path2([Other | Rest], New_Path) ->
	update_path2(Rest, [Other | New_Path]);
update_path2([], New_Path) ->
	lists:reverse(New_Path).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlnselement() | xmlelement()
%%     XML_Text = string()
%% @deprecated Please use {@link document_to_list/1}.
%% @doc Serialize an XML tree to play text.

element_to_string(XML_Element) ->
	exmpp_xml:document_to_list(XML_Element).

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @deprecated Please use {@link encode_entities/1}.
%% @doc Replace sensible characters with entities.

crypt(CData) when is_list(CData) ->
	exmpp_xml:encode_entities(CData);

crypt(CData) when is_binary(CData) ->
	exmpp_xml:encode_entities(binary_to_list(CData)).
