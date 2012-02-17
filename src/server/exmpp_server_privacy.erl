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

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of privacy lists management.

-module(exmpp_server_privacy).

-include("exmpp.hrl").

%% Creating stanza.
-export([
    list_push/2
]).

%%
-export_type([
    list_name/0
]).

-type(list_name() :: binary()).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@Privacy(Name, Attrs, Children),
(
    exxml:element(?NS_PRIVACY, Name, Attrs, Children)
)).


%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @doc Create an `<iq/>' to notify `To' that the privacy list
%% `List_Name' has been created or has changed.
-spec(list_push/2 ::
(
  To        :: exmpp_stanza:to(),
  List_Name :: exmpp_server_privacy:list_name())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

list_push(To, List_Name) ->
    ?IQ_SET(undefined, To, exmpp_utils:random_id(<<"push-">>),
        ?Xmlel@Privacy(<<"query">>, [], [
            ?Xmlel(<<"list">>, [exxml:attr(<<"name">>, List_Name)], [])
        ])
    ).
