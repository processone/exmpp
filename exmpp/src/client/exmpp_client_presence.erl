%% $Id: $

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

%% @spec (StatusOrType, Show) -> PresenceStanza
%%     StatusOrType = atom()
%%     Show = string()
%%     PresenceStanza = exmpp_xml:xmlnselement()
%% @doc Make a `<presence>' packet.
presence(StatusOrType, Show) ->
    PresenceElement = exmpp_xml:set_attributes(
		     #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'presence'},
		     []),
    StatusElement = status_element(StatusOrType),
    PresenceElt2 = exmpp_xml:append_child(PresenceElement, StatusElement),
    ShowElement = exmpp_xml:set_cdata(
		    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = show},
		    Show),
    exmpp_xml:append_child(PresenceElement, ShowElement).

%% -------------------------------------------------------------------
%% Helper functions to generate valid presence packets
%% -------------------------------------------------------------------

%% @spec (StatusOrType) -> StatusElement
%%     StatusOrType = atom()
%%     StatusElement = exmpp_xml:xmlnselement()
%% @doc Generate a valid presence stanza given the presence parameter
status_element(?P_AVAILABLE) ->
    ?NO_ELEMENT;
status_element(?P_UNAVAILABLE) ->
    ?NO_ELEMENT;
status_element(?P_INVISIBLE) ->
    ?NO_ELEMENT;
status_element(?P_AWAY) ->
    set_status_element_value(<<"away">>);
status_element(?P_CHAT) ->
    set_status_element_value(<<"chat">>);
status_element(?P_CHAT) ->
    set_status_element_value(<<"dnd">>);
status_element(?P_CHAT) ->
    set_status_element_value(<<"xa">>);
status_element(_) ->
    erlang:throw().

%% @spec (Value) -> StatusElement
%%     Value = binary()
%%     StatusElement = exmpp_xml:xmlnselement()
set_status_element_value(Value) ->
    exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_CLIENT, name = status},
      Value).
