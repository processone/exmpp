%% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> is a factory for presence packets.
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

%% TODO: Not client specific: Move that into a common directory

-module(exmpp_client_presence).

-include("exmpp.hrl").

-export([presence/2]).

%% TODO: Missing presence wrappers
%% Status, Show, Priority
%% Status, Priority
%% Status, Show, Priority, XMLElement
%% Status, Show, XMLElement
%% Status, Priority, XMLElement

%% types available, unavailable and invisible are handled in the
%% status parameter

%% @spec (ShowOrType, Status) -> PresenceStanza
%%     ShowOrType = atom()
%%     Status = string()
%%     PresenceStanza = exmpp_xml:xmlnselement()
%% @doc Make a `<presence>' packet.
presence(ShowOrType, Status) ->
    PresenceElement = exmpp_xml:set_attributes(
			#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'presence'},
			[]),
    ShowElement = show_element(ShowOrType),
    PresenceElt2 = exmpp_xml:append_child(PresenceElement, ShowElement),
    ShowElement2 = exmpp_xml:set_cdata(
		     #xmlnselement{ns = ?NS_JABBER_CLIENT, name = status},
		     Status),
    exmpp_xml:append_child(PresenceElt2, ShowElement2).

%% -------------------------------------------------------------------
%% Helper functions to generate valid presence packets
%% -------------------------------------------------------------------

%% @spec (ShowOrType) -> StatusElement
%%     ShowOrType = atom()
%%     ShowElement = exmpp_xml:xmlnselement()
%% @doc Generate a valid presence stanza given the presence parameter
show_element(?P_AVAILABLE) ->
    ?NO_ELEMENT;
show_element(?P_UNAVAILABLE) ->
    ?NO_ELEMENT;
show_element(?P_INVISIBLE) ->
    ?NO_ELEMENT;
show_element(?P_AWAY) ->
    set_show_element_value(<<"away">>);
show_element(?P_CHAT) ->
    set_show_element_value(<<"chat">>);
show_element(?P_DND) ->
    set_show_element_value(<<"dnd">>);
show_element(?P_XA) ->
    set_show_element_value(<<"xa">>);
show_element(_) ->
    erlang:throw({presence_error, wrong_show_or_type}).

%% @spec (Value) -> StatusElement
%%     Value = binary()
%%     StatusElement = exmpp_xml:xmlnselement()
set_show_element_value(Value) ->
    exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_CLIENT, name = show},
      Value).
