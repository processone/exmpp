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

%% @author Karim Gemayel <kgemayel@process-one.net>

-module(exmpp_client_disco).

-include("exmpp.hrl").

-define(Xmlel@Disco_Info(Name, Attrs, Children),
(
    exxml:element(?NS_DISCO_INFO, Name, Attrs, Children)
)).

-define(Xmlel@Disco_Items(Name, Attrs, Children),
(
    exxml:element(?NS_DISCO_ITEMS, Name, Attrs, Children)
)).

%% Creation.
-export([
    info/0,
    info/1,
    info/2,
    info/3,
    info/4,
    %%
    items/0,
    items/1,
    items/2,
    items/3,
    items/4
]).

-export_type([
    node_id/0
]).

-type(node_id() :: binary()).

%%

%% @doc Make an <iq/> for a disco#info
-spec(info/0 :: () -> Stanza_IQ_Get::exmpp_stanza:iq_get()).

info() ->
    info(undefined, undefined, iq_id(), undefined).

%% @doc Make an <iq/> for a disco#info
-spec(info/1 ::
(
  To :: exmpp_stanza:to() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

info(To) ->
    info(undefined, To, iq_id(), undefined).

%% @doc Make an <iq/> for a disco#info
-spec(info/2 ::
(
  To     :: exmpp_stanza:to()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

info(To, NodeId) ->
    info(undefined, To, iq_id(), NodeId).

%% @doc Make an <iq/> for a disco#info
-spec(info/3 ::
(
  From   :: exmpp_stanza:from()          | undefined,
  To     :: exmpp_stanza:to()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

info(From, To, NodeId) ->
    info(From, To, iq_id(), NodeId).

%% @doc Make an <iq/> for a disco#info
-spec(info/4 ::
(
  From   :: exmpp_stanza:from()          | undefined,
  To     :: exmpp_stanza:to()            | undefined,
  Id     :: exmpp_stanza:id()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

info(From, To, Id, NodeId) ->
    ?IQ_GET(From, To,
        case Id of
            undefined -> iq_id();
            _         -> Id
        end,
        ?Xmlel@Disco_Info(<<"query">>,
            case NodeId of
                undefined -> [];
                _         -> [exxml:attribute(<<"node">>, NodeId)]
            end,
            [])).

%% @doc Make an <iq/> for a disco#items
-spec(items/0 :: () -> Stanza_IQ_Get::exmpp_stanza:iq_get()).

items() ->
    items(undefined, undefined, iq_id(), undefined).

%% @doc Make an <iq/> for a disco#items
-spec(items/1 ::
(
  To     :: exmpp_stanza:to() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

items(To) ->
    items(undefined, To, iq_id(), undefined).

%% @doc Make an <iq/> for a disco#items
-spec(items/2 ::
(
  To     :: exmpp_stanza:to()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

items(To, NodeId) ->
    items(undefined, To, iq_id(), NodeId).

%% @doc Make an <iq/> for a disco#items
-spec(items/3 ::
(
  From   :: exmpp_stanza:from()          | undefined,
  To     :: exmpp_stanza:to()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

items(From, To, NodeId) ->
    items(From, To, iq_id(), NodeId).

%% @doc Make an <iq/> for a disco#items
-spec(items/4 ::
(
  From   :: exmpp_stanza:from()          | undefined,
  To     :: exmpp_stanza:to()            | undefined,
  Id     :: exmpp_stanza:id()            | undefined,
  NodeId :: exmpp_client_disco:node_id() | undefined)
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

items(From, To, Id, NodeId) ->
    ?IQ_GET(From, To,
        case Id of
            undefined -> iq_id();
            _         -> Id
        end,
        ?Xmlel@Disco_Items(<<"query">>,
            case NodeId of
                undefined -> [];
                _         -> [exxml:attribute(<<"node">>, NodeId)]
            end,
            [])).

%% @doc Generate a random iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

-spec(iq_id/0 :: () -> Id::exmpp_stanza:id()).

iq_id() ->
    exmpp_utils:random_id(<<"iq-">>).
