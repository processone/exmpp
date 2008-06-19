% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helper to do presence
%% common operations.

-module(exmpp_presence).
-vsn('$Revision$').

-include("exmpp.hrl").

% Presence standard attributes.
-export([
  is_presence/1,
  get_type/1,
  get_show/1,
  get_status/1,
  get_priority/1
]).

% --------------------------------------------------------------------
% Presence standard attributes.
% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(Presence)' guard expression.

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_El)                               -> false.

%% @spec (Presence) -> Type
%%     Presence = exmpp_xml:xmlel()
%%     Type = availale | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error | undefined
%% @doc Return the type of the given `<presence/>'.

get_type(Presence) ->
    case exmpp_stanza:get_type(Presence) of
        ""             -> 'available';
        "unavailable"  -> 'unavailable';
        "subscribe"    -> 'subscribe';
        "subscribed"   -> 'subscribed';
        "unsubscribe"  -> 'unsubscribe';
        "unsubscribed" -> 'unsubscribed';
        "probe"        -> 'probe';
        "error"        -> 'error';
        _              -> undefined
    end.

%% @spec (Presence) -> Show | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Show = online | away | chat | dnd | xa | undefined
%% @doc Return the show attribute of the presence.

get_show(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element_by_name(Presence, NS, 'show') of
        undefined ->
            'online';
        Show_El ->
            case string:strip(exmpp_xml:get_cdata(Show_El)) of
                "away" -> 'away';
                "chat" -> 'chat';
                "dnd"  -> 'dnd';
                "xa"   -> 'xa';
                _      -> undefined
            end
    end.

%% @spec (Presence) -> Status | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Status = online | away | chat | dnd | xa | undefined
%% @doc Return the status attribute of the presence.

get_status(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element_by_name(Presence, NS, 'status') of
        undefined ->
            undefined;
        Status_El ->
            exmpp_xml:get_cdata(Status_El)
    end.

%% @spec (Presence) -> Priority
%%     Presence = exmpp_xml:xmlel()
%%     Priority = integer()
%% @doc Return the priority attribute of the presence.

get_priority(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element_by_name(Presence, NS, 'priority') of
        undefined ->
            0;
        Priority_El ->
            case exmpp_xml:get_cdata_as_list(Priority_El) of
                "" -> 0;
                P  -> list_to_integer(P)
            end
    end.
