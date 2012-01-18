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

-define(EMPTY_PRESENCE, exxml:element(<<"presence">>)).

-define(Is_Xmlel_Error(Xmlel_Error),
(
  Xmlel_Error#xmlel.name == <<"error">>
)).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-export_type([
  show/0,
  status/0,
  priority/0
]).

-type(show() :: <<_:48>> | <<_:32>> | <<_:24>> | <<_:16>>).
-type(status() :: binary()).
-type(priority() :: -153 .. 153).

%% --------------------------------------------------------------------
%% Presence creation.
%% --------------------------------------------------------------------


%% @doc Create a presence stanza with given type and status.
-spec(presence/2 ::
(
  Type   :: exmpp_stanza:presence_type(),
  Status :: exmpp_presence:status())
    -> Presence::exmpp_stanza:presence()
).

presence(Type, Status) ->
    set_status(set_type(?EMPTY_PRESENCE, Type), Status).

%% @doc Create a presence stanza to tell that the sender is available.
-spec(available/0 :: () -> Presence_Available::exmpp_stanza:presence_available()).

available() ->
    ?EMPTY_PRESENCE.

%% @doc Create a presence stanza to tell that the sender is not available.
-spec(unavailable/0 :: () -> Presence_Unavailable::exmpp_stanza:presence_unavailable()).

unavailable() ->
    set_type(?EMPTY_PRESENCE, <<"unavailable">>).

%% @doc Create a presence stanza to tell that the sender wants to
%% subscribe to the receiver's presence.
-spec(subscribe/0 :: () -> Presence_Subscribe::exmpp_stanza:presence_subscribe()).

subscribe() ->
    set_type(?EMPTY_PRESENCE, <<"subscribe">>).

%% @doc Create a presence stanza to tell that the receiver was
%% subscribed from the sender's presence.
-spec(subscribed/0 :: () -> Presence_Subscribed::exmpp_stanza:presence_subscribed()).

subscribed() ->
    set_type(?EMPTY_PRESENCE, <<"subscribed">>).

%% @doc Create a presence stanza to tell that the sender wants to
%% unsubscribe to the receiver's presence.
-spec(unsubscribe/0 :: () -> Presence_Unsubscribe::exmpp_stanza:presence_unsubscribe()).

unsubscribe() ->
    set_type(?EMPTY_PRESENCE, <<"unsubscribe">>).

%% @doc Create a presence stanza to tell that the receiver was
%% unsubscribed from the sender's presence.
-spec(unsubscribed/0
  :: () -> Presence_Unsubscribed::exmpp_stanza:presence_unsubscribed()
).

unsubscribed() ->
    set_type(?EMPTY_PRESENCE, <<"unsubscribed">>).

%% @doc Create a probe presence stanza.
-spec(probe/0 :: () -> Presence_Probe::exmpp_stanza:presence_probe()).

probe() ->
    set_type(?EMPTY_PRESENCE, <<"probe">>).


%% @doc Prepare a presence stanza to notify an error.
%%
%% If `Error' is an binary, it must be a standard condition defined by
%% XMPP Core.
-spec(error/2 ::
(
  Presence :: exmpp_stanza:presence(),
  _        :: exmpp_stanza:error_condition() | exmpp_stanza:xmlel_error())
    -> Presence_Error :: exmpp_stanza:presence_error()
).

error(Presence, Error_Condition) when is_binary(Error_Condition) ->
    error(Presence, exmpp_stanza:error(Error_Condition));
error(Presence, Xmlel_Error)
  when ?IS_PRESENCE(Presence) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    exmpp_stanza:reply_with_error(Presence, Xmlel_Error).

%% --------------------------------------------------------------------
%% Presence standard attributes.
%% --------------------------------------------------------------------

%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(El)' guard expression.
-spec(is_presence/1 ::
(
  Presence::exmpp_stanza:presence())
    -> Is_Presence::boolean()
).

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_)                                    -> false.


%% @doc Return the type of the given presence stanza.
-spec(get_type/1 ::
(
  Presence::exmpp_stanza:presence())
    -> Type :: exmpp_stanza:presence_type() | <<_:72>>
).

get_type(Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_stanza:get_type(Presence) of
        undefined -> <<"available">>;
        Type      -> Type
    end.


%% @doc Set the type of the given presence stanza.
-spec(set_type/2 ::
(
  Presence :: exmpp_stanza:presence(),
  Type     :: <<>> | <<_:72>> | exmpp_stanza:presence_type())
    -> Presence :: exmpp_stanza:presence()
).

set_type(Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_attribute(Presence, <<"type">>);
set_type(Presence, <<"available">>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_attribute(Presence, <<"type">>);
set_type(Presence, Type)->
    exmpp_stanza:set_type(Presence, Type).

%% @doc Return the show attribute of the presence.
-spec(get_show/1 ::
(
  Presence::exmpp_stanza:presence())
    -> Show :: exmpp_presence:show() | undefined
).

get_show(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"show">>) of
        undefined ->
            <<"online">>;
        Xmlel_Show ->
            case exmpp_utils:strip(exxml:get_cdata(Xmlel_Show)) of
                <<"away">> -> <<"away">>;
                <<"chat">> -> <<"chat">>;
                <<"dnd">>  -> <<"dnd">>;
                <<"xa">>   -> <<"xa">>;
                _          -> undefined
            end
    end.

%% @doc Set the `<show/>' field of a presence stanza.
%%
%% If `Type' is an empty string or  <<"online">>, the `<show/>'
%% element is removed.

-spec(set_show/2 ::
(
  Presence :: exmpp_stanza:presence(),
  Show     :: <<>> | exmpp_presence:show())
    -> Presence::exmpp_stanza:presence()
).

set_show(Presence, <<>>) when ?IS_PRESENCE(Presence)->
    exxml:remove_element(Presence,  <<"show">>);
set_show(Presence, <<"online">>)
  when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence,  <<"show">>);
set_show(Presence, Show) when is_binary(Show) ->
    exxml:set_or_replace_child(Presence,
        exxml:element(undefined, <<"show">>, [], [exxml:cdata(Show)])
    ).

%% @doc Return the status attribute of the presence.
-spec(get_status/1 ::
(
  Presence::exmpp_stanza:presence())
    -> Status :: exmpp_presence:status() | undefined
).

get_status(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"status">>) of
        undefined    -> undefined;
        Xmlel_Status -> exxml:get_cdata(Xmlel_Status)
    end.

%% @doc Set the `<status/>' field of a presence stanza.
%%
%% If `Status' is an empty binary, the previous
%% status is removed.
-spec(set_status/2 ::
(
  Presence :: exmpp_stanza:presence(),
  Status   :: undefined | <<>> | exmpp_presence:status())
    -> Presence::exmpp_stanza:presence()
).

set_status(Presence, undefined) when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence, <<"status">>);
set_status(Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exxml:remove_element(Presence, <<"status">>);
set_status(Presence, Status) when ?IS_PRESENCE(Presence) ->
    exxml:set_or_replace_child(Presence,
        exxml:element(undefined, <<"status">>, [], [exxml:cdata(Status)])
    ).


%% @doc Return the priority attribute of the presence.
-spec(get_priority/1 ::
(
  Presence::exmpp_stanza:presence())
    -> Priority::exmpp_presence:priority()
).

get_priority(Presence) when ?IS_PRESENCE(Presence) ->
    case exxml:get_element(Presence, <<"priority">>) of
        undefined ->
            0;
        Xmlel_Priority ->
            case exxml:get_cdata(Xmlel_Priority) of
                <<>>  -> 0;
                CData -> list_to_integer(binary_to_list(CData))
            end
    end.

%% @doc Set the `<priority/>' field of a presence stanza.
-spec(set_priority/2 ::
(
  Presence :: exmpp_stanza:presence(),
  Priority :: exmpp_presence:priority())
    -> Presence::exmpp_stanza:presence()
).

set_priority(Presence, Priority)
  when ?IS_PRESENCE(Presence) andalso is_integer(Priority) ->
    exxml:set_or_replace_child(Presence,
        exxml:element(undefined, <<"priority">>, [],
            [exxml:cdata(list_to_binary(integer_to_list(Priority)))])
    ).

