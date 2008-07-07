% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helper to do presence
%% common operations.

-module(exmpp_presence).
-vsn('$Revision$').

-include("exmpp.hrl").

% Presence creation.
-export([
  available/0,
  unavailable/0,
  probe/0
]).

% Presence standard attributes.
-export([
  is_presence/1,
  get_type/1,
  get_show/1,
  set_show/2,
  get_status/1,
  set_status/2,
  get_priority/1,
  set_priority/2
]).

% --------------------------------------------------------------------
% Presence creation.
% --------------------------------------------------------------------

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the sender is available.

available() ->
    #xmlel{
      ns = ?NS_JABBER_CLIENT,
      name = 'presence'
    }.

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the sender is not available.

unavailable() ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "unavailable"),
    #xmlel{
      ns = ?NS_JABBER_CLIENT,
      name = 'presence',
      attrs = Attrs1
    }.

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a probe `<presence/>'.

probe() ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "probe"),
    #xmlel{
      ns = ?NS_JABBER_CLIENT,
      name = 'presence',
      attrs = Attrs1
    }.

% --------------------------------------------------------------------
% Presence standard attributes.
% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(Presence)' guard expression.

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_El)                                  -> false.

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
            % TODO: Fix me: we should not need to convert back to list
            % just to strip the data.
            case string:strip(exmpp_xml:get_cdata_as_list(Show_El)) of
                "away" -> 'away';
                "chat" -> 'chat';
                "dnd"  -> 'dnd';
                "xa"   -> 'xa';
                _      -> undefined
            end
    end.

%% @spec (Presence, Show) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Show = atom()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<show/>' field of a presence stanza.

set_show(#xmlel{ns = NS} = Presence, Show) when ?IS_PRESENCE(Presence) ->
    case Show of
        'away' -> ok;
        'chat' -> ok;
        'dnd'  -> ok;
        'xa'   -> ok;
        _      -> throw({presence, set_show, invalid_show, Show})
    end,
    New_Show_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'show'}, Show),
    case exmpp_xml:get_element_by_name(Presence, NS, 'show') of
        undefined ->
            exmpp_xml:prepend_child(Presence, New_Show_El);
        Show_El ->
            exmpp_xml:replace_child(Presence, Show_El, New_Show_El)
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

%% @spec (Presence, Status) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Status = string()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<status/>' field of a presence stanza.

set_status(#xmlel{ns = NS} = Presence, Status) when ?IS_PRESENCE(Presence) ->
    New_Status_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'status'},
      Status),
    case exmpp_xml:get_element_by_name(Presence, NS, 'status') of
        undefined ->
            exmpp_xml:prepend_child(Presence, New_Status_El);
        Status_El ->
            exmpp_xml:replace_child(Presence, Status_El, New_Status_El)
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

%% @spec (Presence, Priority) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Priority = integer()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<priority/>' field of a presence stanza.

set_priority(#xmlel{ns = NS} = Presence, Priority)
  when ?IS_PRESENCE(Presence) ->
    New_Priority_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'priority'},
      Priority),
    case exmpp_xml:get_element_by_name(Presence, NS, 'priority') of
        undefined ->
            exmpp_xml:prepend_child(Presence, New_Priority_El);
        Priority_El ->
            exmpp_xml:replace_child(Presence, Priority_El, New_Priority_El)
    end.
