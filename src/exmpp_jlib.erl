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

-export([
  jid_to_string/1,
  string_to_jid/1,
  make_jid/3
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

% --------------------------------------------------------------------
% JID handling.
% --------------------------------------------------------------------

jid_to_string(#jid{user = User, server = Server, resource = Res}) ->
    jid_to_string(User, Server, Res).

jid_to_string(Node, Server, Res) ->
    S1 = case Node of
        "" -> "";
        _  -> Node ++ "@"
    end,
    S2 = S1 ++ Server,
    S3 = case Res of
        "" -> S2;
        _  -> S2 ++ "/" ++ Res
    end,
    S3.

string_to_jid(Jid_S) ->
    string_to_jid1(Jid_S, "").

% Parse user/node.
string_to_jid1([$@ | _Jid_S], "") ->
    {error, node_or_user_expected};
string_to_jid1([$@ | Jid_S], Node) ->
    string_to_jid2(Jid_S, lists:reverse(Node), "");
string_to_jid1([$/ | _Jid_S], "") ->
    {error, server_expected};
string_to_jid1([$/ | Jid_S], Node) ->
    string_to_jid3(Jid_S, "", lists:reverse(Node), "");
string_to_jid1([C  | Jid_S], Node) ->
    string_to_jid1(Jid_S, [C | Node]);
string_to_jid1([], Node) ->
    make_jid("", lists:reverse(Node), "").

% Parser server.
string_to_jid2([$@ | _Jid_S], _Node, _Server) ->
    {error, at_character_not_allowed};
string_to_jid2([$/ | _Jid_S], _Node, "") ->
    {error, server_expected};
string_to_jid2([$/ | Jid_S], Node, Server) ->
    string_to_jid3(Jid_S, Node, lists:reverse(Server), "");
string_to_jid2([C  | Jid_S], Node, Server) ->
    string_to_jid2(Jid_S, Node, [C | Server]);
string_to_jid2([], Node, Server) ->
    make_jid(Node, lists:reverse(Server), "").

% Parse resource.
string_to_jid3([C  | Jid_S], Node, Server, Res) ->
    string_to_jid3(Jid_S, Node, Server, [C | Res]);
string_to_jid3([], Node, Server, Res) ->
    make_jid(Node, Server, lists:reverse(Res)).

% Make #jid{}.
% XXX The original function uses `stringprep'. This one should do the same
% but `stringprep' must be renamed/reworked to become, eg, `exmpp_stringprep'.
make_jid(Node, Server, Resource) ->
    #jid{
      user = Node,
      server = Server,
      resource = Resource,
      luser = Node,
      lserver = Server,
      lresource = Resource
    }.
