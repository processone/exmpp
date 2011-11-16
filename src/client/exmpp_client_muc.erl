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

%% @author Pablo Polvorin <pablo.polvorin@process-one.net>
%% @doc Helper functions for generating MUC related stanzas. 
%%      Very incomplete, now it only generates kick and ban stanzas. 

-module(exmpp_client_muc).


-include("exmpp.hrl").

-export([kick/2, 
         kick/3,
         ban/2,
         ban/3,
         get_banlist/1,
         update_banlist/2
         ]).

-type(ban_item() :: {binary(), outcast | none | binary()} | {binary(), outcast | none | binary(), binary() | string()}).


-spec kick(Room :: binary(), Nick :: binary()) -> exml:xmlel().
kick(Room, Nick) ->
    kick(Room, Nick, <<>>).

-spec kick(Room :: binary(), Nick :: binary(), Reason :: binary()) -> exml:xmlel().
kick(Room, Nick, Reason) ->
    exmpp_stanza:set_recipient(
        exmpp_iq:set({xmlel, <<"query">>, [{<<"xmlns">>, ?NS_MUC_ADMIN}],   [
			{xmlel, <<"item">>, [{<<"nick">>, Nick}, {<<"role">>, <<"none">>}], [
				{xmlel, <<"reason">>, [], [{cdata, Reason}]}
			]}]}),Room).



-spec ban(Room :: binary(), JID :: binary()) -> exml:xmlel().
ban(Room, JID) ->
    ban(Room, JID, <<>>).

-spec ban(Room :: binary(), JID :: binary(), Reason :: binary()) -> exml:xmlel().
ban(Room, JID, Reason) ->
    update_banlist(Room, [{JID, outcast, Reason}]).


-spec get_banlist(Room :: binary()) -> exml:xmlel().
get_banlist(Room) ->
    exmpp_stanza:set_recipient(
	    exmpp_iq:get({xmlel, <<"query">>, [{<<"xmlns">>, ?NS_MUC_ADMIN}], 
			    [{xmlel, <<"item">>, [{<<"affiliation">>, <<"outcast">>}], []}]}), Room ).
    

-spec update_banlist(Room :: binary(), BanList :: [ban_item()]) -> exml:xmlel().
update_banlist(Room, BanList) ->
     exmpp_stanza:set_recipient(
	     exmpp_iq:set({xmlel, <<"query">>, [{<<"xmlns">>, ?NS_MUC_ADMIN}], 
			     [ban_to_item(Ban) || Ban <- BanList]}), Room).


ban_to_item({JID, Affiliation}) ->
    ban_to_item({JID, Affiliation, <<>>});
ban_to_item({JID, Affiliation, Reason}) ->
	{xmlel, <<"item">>, 
		[{<<"jid">>, JID}, {<<"affiliation">>, affiliation_to_binary(Affiliation)}], 
	        [{xmlel, <<"reason">>, [], [{cdata, Reason}]}]}.


affiliation_to_binary(outcast) -> <<"outcast">>;
affiliation_to_binary(none) -> <<"none">>;
affiliation_to_binary(A) when is_binary(A) -> A.
