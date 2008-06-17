%% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc

%% The module <strong>{@module}</strong> implements XMPP roster
%% management packet generation.
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>

-module(exmpp_client_roster).

-include("exmpp.hrl").

-export([get_roster/0, get_roster/1,
	set_item/3, set_item/4]).

%% @spec () -> Roster_Iq
%%     Roster_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to retrieve user roster.
%%
%% The stanza `id' is generated automatically.
get_roster() ->
    get_roster(roster_id()).

%% @spec (Id) -> Roster_Iq
%%     Id = string()
%%     Roster_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to retrieve user roster.
get_roster(Id) ->
    Query = #xmlel{ns = ?NS_JABBER_ROSTER, name = 'query'},
    Iq = exmpp_xml:set_attributes(
	   #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
	   [{'type', "get"}, {'id', Id}]),
    exmpp_xml:append_child(Iq, Query).

%% @spec (ContactJID, Groups, Nick) -> Roster_Iq
%%     ContactJID = string()
%%     Groups = [string()]
%%     Nick = string()
%%     Roster_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry
set_item(ContactJID, Groups, Nick) ->
    set_item(roster_id(), ContactJID, Groups, Nick).

%% @spec (Id, ContactJID, Groups, Nick) -> Roster_Iq
%%     Id = string()
%%     ContactJID = string()
%%     Groups = [string()]
%%     Nick = string()
%%     Roster_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry
%% TODO: Groups support
set_item(Id, ContactJID, _Groups, Nick) ->
    Item = exmpp_xml:set_attributes(
	     #xmlel{name = 'item'},
	     [{'name', Nick}, {'jid', ContactJID}]),
    Query = #xmlel{ns = ?NS_JABBER_ROSTER, name = 'query'},
    Query2 = exmpp_xml:append_child(Query, Item),
    Iq = exmpp_xml:set_attributes(
	   #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
	   [{'type', "set"}, {'id', Id}]),
    exmpp_xml:append_child(Iq, Query2).
    
%% @spec () -> Roster_ID
%%     Roster_ID = string()
%% @doc Generate a random roster iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

roster_id() ->
	"rost-" ++ integer_to_list(random:uniform(65536 * 65536)).
