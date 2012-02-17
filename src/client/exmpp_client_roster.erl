%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%%
%% The module <strong>{@module}</strong> implements XMPP roster
%% management packet generation.

-module(exmpp_client_roster).

-include("exmpp.hrl").

-export([
    get_roster/0,
    get_roster/1,
    set_item/3,
    set_item/4
]).

%%
-export_type([
    group/0,
    groups/0,
    nick/0,
    jid/0
]).

-type(group() :: binary()).
-type(groups() :: [Group::exmpp_client_roster:group()]).
-type(nick() :: binary()).
-type(jid() :: binary()).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@Roster(Name, Attrs, Children),
(
    exxml:element(?NS_ROSTER, Name, Attrs, Children)
)).


%% @doc Make an `<iq>' to retrieve user roster.
%%
%% The stanza `id' is generated automatically.

-spec(get_roster/0 :: () -> Stanza_IQ_Get::exmpp_stanza:iq_get()).

get_roster() ->
    get_roster(roster_id()).

%% @doc Make an `<iq>' to retrieve user roster.

-spec(get_roster/1 ::
(
  Id::exmpp_stanza:id())
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

get_roster(Id) ->
    ?IQ_GET(undefined, undefined, Id,
        ?Xmlel@Roster(<<"query">>, [], [])).

%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry

-spec(set_item/3 ::
(
  Contact :: exmpp_client_roster:jid(),
  Groups  :: exmpp_client_roster:groups(),
  Nick    :: exmpp_client_roster:nick() | undefined)
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

set_item(Contact, Groups, Nick) ->
    set_item(roster_id(), Contact, Groups, Nick).

%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry

-spec(set_item/4 ::
(
  Id      :: exmpp_stanza:id(),
  Contact :: exmpp_client_roster:jid(),
  Groups  :: exmpp_client_roster:groups(),
  Nick    :: exmpp_client_roster:nick() | undefined)
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

set_item(Id, Contact, Groups, Nick) ->
    ?IQ_SET(undefined, undefined, Id,
        ?Xmlel@Roster(<<"query">>, [], [
            ?Xmlel(<<"item">>,
                [exxml:attr(<<"jid">>, Contact) |
                 case Nick of
                    undefined -> [];
                    _         -> [exxml:attr(<<"name">>, Nick)]
                 end],
                [?Xmlel(<<"group">>, [], [exxml:cdata(Group)]) || Group <- Groups])
        ])).

%% @doc Generate a random roster iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

-spec(roster_id/0 :: () -> Id::exmpp_stanza:id()).

roster_id() ->
    exmpp_utils:random_id(<<"rost-">>).
