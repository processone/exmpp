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
%% The module <strong>{@module}</strong> provides helpers to do presence
%% common operations.

-module(exmpp_presence).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Presence creation.
-export([
	 presence/2,
	 available/0,
	 unavailable/0,
	 subscribe/0,
	 subscribed/0,
	 unsubscribe/0,
	 unsubscribed/0,
	 probe/0,
	 error/2
	]).

%% Presence standard attributes.
-export([
	 is_presence/1,
	 get_type/1,
	 set_type/2,
	 get_show/1,
	 set_show/2,
	 get_status/1,
	 set_status/2,
	 get_priority/1,
	 set_priority/2
	]).

-define(EMPTY_PRESENCE, {xmlel, <<"presence">>, [], []}).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-type(presencetype() :: binary()).
%      <<"available">>    |
%      <"unavailable">>  |
%      <<"subscribe">>    |
%      <<"subscribed">>   |
%      <<"unsubscribe">>  |
%      <<"unsubscribed">> |
%      <<"probe">>        |
%      <<"error">>
%     ).

-type(presenceshow() :: binary()).
%      online |
%      away   |
%      chat   |
%      dnd    |
%      xa
%     ).

%% --------------------------------------------------------------------
%% Presence creation.
%% --------------------------------------------------------------------

%% @spec (Type, Status) -> Presence
%%     Type = presencetype()
%%     Status = binary()
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza with given type and status.

-spec presence
(presencetype(), binary() ) -> exxml:xmlel().

presence(Type, Status) ->
    set_status(set_type(?EMPTY_PRESENCE, Type), Status).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the sender is available.

-spec available () -> exxml:xmlel().

available() ->
    ?EMPTY_PRESENCE.

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the sender is not available.

-spec unavailable () -> exxml:xmlel().

unavailable() ->
    set_type(?EMPTY_PRESENCE, <<"unavailable">>).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the sender wants to
%% subscribe to the receiver's presence.

-spec subscribe () -> exxml:xmlel().

subscribe() ->
    set_type(?EMPTY_PRESENCE, <<"subscribe">>).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the receiver was
%% subscribed from the sender's presence.

-spec subscribed () -> exxml:xmlel().

subscribed() ->
    set_type(?EMPTY_PRESENCE, <<"subscribed">>).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the sender wants to
%% unsubscribe to the receiver's presence.

-spec unsubscribe () -> exxml:xmlel().

unsubscribe() ->
    set_type(?EMPTY_PRESENCE, <<"unsubscribe">>).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a presence stanza to tell that the receiver was
%% unsubscribed from the sender's presence.

-spec unsubscribed () -> exxml:xmlel().

unsubscribed() ->
    set_type(?EMPTY_PRESENCE, <<"unsubscribed">>).

%% @spec () -> Presence
%%     Presence = exxml:xmlel()
%% @doc Create a probe presence stanza.

-spec probe () -> exxml:xmlel().

probe() ->
    set_type(?EMPTY_PRESENCE, <<"probe">>).

%% @spec (Presence, Error) -> New_Presence
%%     Presence = exxml:xmlel()
%%     Error = exxml:xmlel() | binary()
%%     New_Presence = exxml:xmlel()
%% @doc Prepare a presence stanza to notify an error.
%%
%% If `Error' is an binary, it must be a standard condition defined by
%% XMPP Core.

-spec error
(exxml:xmlel(), exxml:xmlel() | binary()) -> exxml:xmlel().

error(Presence, Condition) when is_binary(Condition) ->
    Error = exmpp_stanza:error(Condition),
    error(Presence, Error);
error(Presence, Error) when ?IS_PRESENCE(Presence) ->
    exmpp_stanza:reply_with_error(Presence, Error).

%% --------------------------------------------------------------------
%% Presence standard attributes.
%% --------------------------------------------------------------------

%% @spec (El) -> boolean
%%     El = exxml:xmlel()
%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(El)' guard expression.

-spec is_presence (exxml:xmlel()) -> boolean().

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_El)                                  -> false.

%% @spec (Presence) -> Type
%%     Presence = exxml:xmlel()
%%     Type = binary() | undefined
%% @doc Return the type of the given presence stanza.

-spec get_type (exxml:xmlel()) -> presencetype().

get_type(Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_stanza:get_type(Presence) of
        undefined          -> <<"available">>;
        T -> T
    end.
%% @spec (Presence, Type) -> New_Presence
%%     Presence = exxml:xmlel()
%%     Type = binary()
%%     New_Presence = exxml:xmlel()
%% @doc Set the type of the given presence stanza.

-spec set_type (exxml:xmlel(), presencetype() ) -> exxml:xmlel().

set_type(Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_attribute(Presence, <<"type">>);
set_type(Presence, <<"available">>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_attribute(Presence, <<"type">>);

set_type(Presence, Type)->
    exmpp_stanza:set_type(Presence, Type).


%% @spec (Presence) -> Show | undefined
%%     Presence = exxml:xmlel()
%%     Show = binary() 
%% @doc Return the show attribute of the presence.

-spec get_show
(exxml:xmlel()) -> presenceshow() | undefined.

get_show(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"show">>) of
        undefined ->
            <<"online">>;
        Show_El ->
            case exmpp_utils:strip(exxml:get_cdata(Show_El)) of
                <<"away">> -> <<"away">>;
                <<"chat">> -> <<"chat">>;
                <<"dnd">>  -> <<"dnd">>;
                <<"xa">>   -> <<"xa">>;
                _          -> undefined
            end
    end.

%% @spec (Presence, Show) -> New_Presence
%%     Presence = exxml:xmlel()
%%     Show = binary()
%%     New_Presence = exxml:xmlel()
%% @doc Set the `<show/>' field of a presence stanza.
%%
%% If `Type' is an empty string or  <<"online">>, the `<show/>'
%% element is removed.

-spec set_show
(exxml:xmlel(), presenceshow() | binary() ) -> exxml:xmlel().

set_show(Presence, <<>>) when ?IS_PRESENCE(Presence)->
    exxml:remove_element(Presence,  <<"show">>);
set_show(Presence, <<"online">>)
  when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence,  <<"show">>);
set_show(Presence, Show) when is_binary(Show) ->
    ShowEl = {xmlel, <<"show">>, [], [{cdata, Show}]},
    exxml:set_or_replace_child(Presence, ShowEl).

%% @spec (Presence) -> Status | undefined
%%     Presence = exxml:xmlel()
%%     Status = binary()
%% @doc Return the status attribute of the presence.

-spec get_status
(exxml:xmlel()) -> binary() | undefined.

get_status(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"status">>) of
        undefined ->
            undefined;
        Status_El ->
            exxml:get_cdata(Status_El)
    end.

%% @spec (Presence, Status) -> New_Presence
%%     Presence = exxml:xmlel()
%%     Status = binary()
%%     New_Presence = exxml:xmlel()
%% @doc Set the `<status/>' field of a presence stanza.
%%
%% If `Status' is an empty binary, the previous
%% status is removed.

-spec set_status
(exxml:xmlel(), binary() | undefined) -> exxml:xmlel().

set_status(Presence, undefined)
  when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence,  <<"status">>);
set_status(Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence, <<"status">>);
set_status(Presence, Status) when ?IS_PRESENCE(Presence) ->
    StatusEl = {xmlel, <<"status">>, [], [{cdata, Status}]},
    exxml:set_or_replace_child(Presence, StatusEl).

%% @spec (Presence) -> Priority
%%     Presence = exxml:xmlel()
%%     Priority = integer()
%% @doc Return the priority attribute of the presence.

-spec get_priority
(exxml:xmlel()) -> integer().

get_priority(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"priority">>) of
        undefined ->
            0;
        Priority_El ->
            case exxml:get_cdata(Priority_El) of
                <<>> -> 0;
                P  -> list_to_integer(binary_to_list(P))
            end
    end.

%% @spec (Presence, Priority) -> New_Presence
%%     Presence = exxml:xmlel()
%%     Priority = integer()
%%     New_Presence = exxml:xmlel()
%% @doc Set the `<priority/>' field of a presence stanza.

-spec set_priority
(exxml:xmlel(), integer()) -> exxml:xmlel().

set_priority(Presence, Priority)
  when ?IS_PRESENCE(Presence) andalso is_integer(Priority) ->
	New_Priority_El = {xmlel, <<"priority">>, [], [{cdata, list_to_binary(integer_to_list(Priority))}]},
	exxml:set_or_replace_child(Presence, New_Priority_El).
