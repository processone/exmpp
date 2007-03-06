% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_jlib).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  exchange_attrs_from_and_to_in_list/1,
  exchange_attrs_from_and_to/1,
  rename_attr_to_to_from_in_list/1,
  rename_attr_to_to_from/1
]).

% --------------------------------------------------------------------
% Helpers to handle common attribute operations.
% --------------------------------------------------------------------

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%% @doc Exchange values of attributes `From' and `To' in the list.
%%
%% This function expects that names are encoded as atom().

exchange_attrs_from_and_to_in_list(Attrs) ->
    To = exmpp_xml:get_attribute_from_list(Attrs, 'to'),
    From = exmpp_xml:get_attribute_from_list(Attrs, 'from'),
    Attrs1 = exmpp_xml:set_attribute_in_list(Attrs, 'to', From),
    Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'from', To),
    Attrs2.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%%     New_XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%% @doc Exchange values of attributes `From' and `To'.
%%
%% This function expects that names are encoded as atom().

exchange_attrs_from_and_to(#xmlnselement{attrs = Attrs} = XML_Element) ->
    New_Attrs = exchange_attrs_from_and_to_in_list(Attrs),
    XML_Element#xmlnselement{attrs = New_Attrs};

exchange_attrs_from_and_to(#xmlelement{attrs = Attrs} = XML_Element) ->
    New_Attrs = exchange_attrs_from_and_to_in_list(Attrs),
    XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%% @doc Rename `To' attribute to `From' in the list.
%%
%% This function expects that names are encoded as atom().

rename_attr_to_to_from_in_list(Attrs) ->
    To = exmpp_xml:get_attribute_from_list(Attrs, 'to'),
    Attrs1 = exmpp_xml:remove_attribute_from_list(Attrs, 'to'),
    Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'from', To),
    Attrs2.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%%     New_XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%% @doc Rename `To' attribute to `From'.
%%
%% This function expects that names are encoded as atom().

rename_attr_to_to_from(#xmlnselement{attrs = Attrs} = XML_Element) ->
    New_Attrs = rename_attr_to_to_from_in_list(Attrs),
    XML_Element#xmlnselement{attrs = New_Attrs};

rename_attr_to_to_from(#xmlelement{attrs = Attrs} = XML_Element) ->
    New_Attrs = rename_attr_to_to_from_in_list(Attrs),
    XML_Element#xmlelement{attrs = New_Attrs}.
