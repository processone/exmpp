%% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc

%% The module <strong>{@module}</strong> implements XMPP roster
%% management packet generation.
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>

-module(exmpp_client_roster).

-include("exmpp.hrl").

-export([get_roster/0, get_roster/1]).

%% @spec () -> Roster_Iq
%%     Roster_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to retrieve user roster.
%%
%% The stanza `id' is generated automatically.
get_roster() ->
    get_roster(roster_id()).

%% @spec (Id) -> Roster_Iq
%%     Id = string()
%%     Roster_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to retrieve user roster.
get_roster(Id) ->
    Query = #xmlnselement{ns = ?NS_JABBER_ROSTER, name = 'query', children = []},
    Iq = exmpp_xml:set_attributes(
	   #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'},
	   [{'type', "get"}, {'id', Id}]),
    exmpp_xml:append_child(Iq, Query).

%% @spec () -> Roster_ID
%%     Roster_ID = string()
%% @doc Generate a random roster iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

roster_id() ->
	"rost-" ++ integer_to_list(random:uniform(65536 * 65536)).
