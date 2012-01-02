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

-export([get_roster/0, get_roster/1,
	 set_item/3, set_item/4]).

%% @spec () -> Roster_Iq
%%     Roster_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to retrieve user roster.
%%
%% The stanza `id' is generated automatically.
get_roster() ->
    get_roster(roster_id()).

%% @spec (Id) -> Roster_Iq
%%     Id = binary()
%%     Roster_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to retrieve user roster.
get_roster(Id) ->
   Query = {xmlel, <<"query">>, [{<<"xmlns">>, ?NS_ROSTER}], []},
   {xmlel, <<"iq">>, [{<<"type">>, <<"get">>}, {<<"id">>, Id}], [Query]}.

%% @spec (ContactJID, Groups, Nick) -> Roster_Iq
%%     ContactJID = binary()
%%     Groups = [binary()]
%%     Nick = binary()
%%     Roster_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry
set_item(ContactJID, Groups, Nick) ->
    set_item(roster_id(), ContactJID, Groups, Nick).

%% @spec (Id, ContactJID, Groups, Nick) -> Roster_Iq
%%     Id = binary()
%%     ContactJID = binary()
%%     Groups = [binary()]
%%     Nick = binary()
%%     Roster_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to update a roster item. This function is used
%% both to create a roster item and to update an roster entry
set_item(Id, ContactJID, Groups, Nick) ->
    Item = {xmlel, <<"item">>, [{<<"name">>, Nick}, {<<"jid">>, ContactJID}],
		[{xmlel, <<"group">>, [], [{cdata, Gr}]} || Gr <- Groups]},
    Query = {xmlel, <<"query">>, [{<<"xmlns">>, ?NS_ROSTER}], [Item]},
    {xmlel, <<"iq">>, [{<<"type">>, <<"set">>}, {<<"id">>, Id}], [Query]}.

%% @spec () -> Roster_ID
%%     Roster_ID = binary()
%% @doc Generate a random roster iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

roster_id() ->
	exmpp_utils:random_id(<<"rost-">>).
