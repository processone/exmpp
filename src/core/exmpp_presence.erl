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
  subscribe/0,
  subscribed/0,
  unsubscribe/0,
  unsubscribed/0,
  probe/0,
  error/0
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
    set_type(available(), "unavailable").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the sender wants to
%% subscribe to the receiver's presence.

subscribe() ->
    set_type(available(), "subscribe").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the receiver was
%% subscribed from the sender's presence.

subscribed() ->
    set_type(available(), "subscribed").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the sender wants to
%% unsubscribe to the receiver's presence.

unsubscribe() ->
    set_type(available(), "unsubscribe").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a `<presence/>' to tell that the receiver was
%% unsubscribed from the sender's presence.

unsubscribed() ->
    set_type(available(), "unsubscribed").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a probe `<presence/>'.

probe() ->
    set_type(available(), "probe").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create an error `<presence/>'.

error() ->
    set_type(available(), "error").

% --------------------------------------------------------------------
% Presence standard attributes.
% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(El)' guard expression.

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_El)                                  -> false.

%% @spec (Presence) -> Type
%%     Presence = exmpp_xml:xmlel()
%%     Type = available | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error | undefined
%% @doc Return the type of the given `<presence/>'.

get_type(Presence) when ?IS_PRESENCE(Presence) ->
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

%% @spec (Presence, Type) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Type = available | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the type of the given `<presence/>'.

set_type(Presence, "") when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, 'available') when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, "available") when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');

set_type(Presence, Type) when is_atom(Type) ->
    set_type(Presence, atom_to_list(Type));
set_type(Presence, Type) when ?IS_PRESENCE(Presence) ->
    exmpp_stanza:set_type(Presence, Type).

%% @spec (Presence) -> Show | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Show = online | away | chat | dnd | xa | undefined
%% @doc Return the show attribute of the presence.

get_show(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element(Presence, NS, 'show') of
        undefined ->
            'online';
        Show_El ->
            case exmpp_utils:strip(exmpp_xml:get_cdata(Show_El)) of
                "away" -> 'away';
                "chat" -> 'chat';
                "dnd"  -> 'dnd';
                "xa"   -> 'xa';
                _      -> undefined
            end
    end.

%% @spec (Presence, Show) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Show = nil() | online | away | chat | dnd | xa
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<show/>' field of a presence stanza.
%%
%% If `Type' is an empty string or the atom `online', the `<show/>'
%% element is removed.

set_show(#xmlel{ns = NS} = Presence, "") when ?IS_PRESENCE(Presence)->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, 'online') when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, Show) when ?IS_PRESENCE(Presence) ->
    case Show of
        'away' -> ok;
        'chat' -> ok;
        'dnd'  -> ok;
        'xa'   -> ok;
        _      -> throw({presence, set_show, invalid_show, Show})
    end,
    New_Show_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'show'}, Show),
    case exmpp_xml:get_element(Presence, NS, 'show') of
        undefined ->
            exmpp_xml:prepend_child(Presence, New_Show_El);
        Show_El ->
            exmpp_xml:replace_child(Presence, Show_El, New_Show_El)
    end.

%% @spec (Presence) -> Status | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Status = binary()
%% @doc Return the status attribute of the presence.

get_status(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element(Presence, NS, 'status') of
        undefined ->
            undefined;
        Status_El ->
            exmpp_xml:get_cdata(Status_El)
    end.

%% @spec (Presence, Status) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Status = string() | binary()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<status/>' field of a presence stanza.

set_status(#xmlel{ns = NS} = Presence, Status) when ?IS_PRESENCE(Presence) ->
    New_Status_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'status'},
      Status),
    case exmpp_xml:get_element(Presence, NS, 'status') of
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
    case exmpp_xml:get_element(Presence, NS, 'priority') of
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
    case exmpp_xml:get_element(Presence, NS, 'priority') of
        undefined ->
            exmpp_xml:prepend_child(Presence, New_Priority_El);
        Priority_El ->
            exmpp_xml:replace_child(Presence, Priority_El, New_Priority_El)
    end.
