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

-define(EMPTY_PRESENCE, #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence'}).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-type(presencetype() ::
      available    |
      unavailable  |
      subscribe    |
      subscribed   |
      unsubscribe  |
      unsubscribed |
      probe        |
      error
     ).

-type(presenceshow() ::
      online |
      away   |
      chat   |
      dnd    |
      xa
     ).

%% --------------------------------------------------------------------
%% Presence creation.
%% --------------------------------------------------------------------

%% @spec (Type, Status) -> Presence
%%     Type = available | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error
%%     Status = string() | binary()
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza with given type and status.

-spec presence
(presencetype(), binary() | string()) -> exmpp_xml:xmlel().

presence(Type, Status) ->
    set_status(set_type(?EMPTY_PRESENCE, Type), Status).

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the sender is available.

-spec available
() -> exmpp_xml:xmlel().

available() ->
    ?EMPTY_PRESENCE.

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the sender is not available.

-spec unavailable
() -> exmpp_xml:xmlel().

unavailable() ->
    set_type(?EMPTY_PRESENCE, "unavailable").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the sender wants to
%% subscribe to the receiver's presence.

-spec subscribe
() -> exmpp_xml:xmlel().

subscribe() ->
    set_type(?EMPTY_PRESENCE, "subscribe").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the receiver was
%% subscribed from the sender's presence.

-spec subscribed
() -> exmpp_xml:xmlel().

subscribed() ->
    set_type(?EMPTY_PRESENCE, "subscribed").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the sender wants to
%% unsubscribe to the receiver's presence.

-spec unsubscribe
() -> exmpp_xml:xmlel().

unsubscribe() ->
    set_type(?EMPTY_PRESENCE, "unsubscribe").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a presence stanza to tell that the receiver was
%% unsubscribed from the sender's presence.

-spec unsubscribed
() -> exmpp_xml:xmlel().

unsubscribed() ->
    set_type(?EMPTY_PRESENCE, "unsubscribed").

%% @spec () -> Presence
%%     Presence = exmpp_xml:xmlel()
%% @doc Create a probe presence stanza.

-spec probe
() -> exmpp_xml:xmlel().

probe() ->
    set_type(?EMPTY_PRESENCE, "probe").

%% @spec (Presence, Error) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel() | atom()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Prepare a presence stanza to notify an error.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec error
(exmpp_xml:xmlel(), exmpp_xml:xmlel() | atom()) -> exmpp_xml:xmlel().

error(Presence, Condition) when is_atom(Condition) ->
    Error = exmpp_stanza:error(Presence#xmlel.ns, Condition),
    error(Presence, Error);
error(Presence, Error) when ?IS_PRESENCE(Presence) ->
    exmpp_stanza:reply_with_error(Presence, Error).

%% --------------------------------------------------------------------
%% Presence standard attributes.
%% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a presence.
%%
%% You should probably use the `IS_PRESENCE(El)' guard expression.

-spec is_presence
(exmpp_xml:xmlel()) -> bool().

is_presence(Presence) when ?IS_PRESENCE(Presence) -> true;
is_presence(_El)                                  -> false.

%% @spec (Presence) -> Type
%%     Presence = exmpp_xml:xmlel()
%%     Type = available | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error | undefined
%% @doc Return the type of the given presence stanza.

-spec get_type
(exmpp_xml:xmlel()) -> presencetype() | undefined.

get_type(Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_stanza:get_type(Presence) of
        undefined          -> 'available';
        <<"unavailable">>  -> 'unavailable';
        <<"subscribe">>    -> 'subscribe';
        <<"subscribed">>   -> 'subscribed';
        <<"unsubscribe">>  -> 'unsubscribe';
        <<"unsubscribed">> -> 'unsubscribed';
        <<"probe">>        -> 'probe';
        <<"error">>        -> 'error';
        _                  -> undefined
    end.

%% @spec (Presence, Type) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Type = available | unavailable | subscribe | subscribed | unsubscribe | unsubscribed | probe | error | binary() | string()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the type of the given presence stanza.

-spec set_type
(exmpp_xml:xmlel(), presencetype() | binary() | string()) -> exmpp_xml:xmlel().

set_type(Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, "") when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, 'available') when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, <<"available">>) when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');
set_type(Presence, "available") when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_attribute(Presence, 'type');

set_type(Presence, Type) when is_binary(Type) ->
    set_type(Presence, binary_to_list(Type));
set_type(Presence, Type) when is_list(Type) ->
    set_type(Presence, list_to_atom(Type));

set_type(Presence, Type) when ?IS_PRESENCE(Presence), is_atom(Type) ->
    Type_B = case Type of
		 'unavailable'  -> <<"unavailable">>;
		 'subscribe'    -> <<"subscribe">>;
		 'subscribed'   -> <<"subscribed">>;
		 'unsubscribe'  -> <<"unsubscribe">>;
		 'unsubscribed' -> <<"unsubscribed">>;
		 'probe'        -> <<"probe">>;
		 'error'        -> <<"error">>;
		 _              -> throw({presence, set_type, invalid_type, Type})
	     end,
    exmpp_stanza:set_type(Presence, Type_B).

%% @spec (Presence) -> Show | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Show = online | away | chat | dnd | xa | undefined
%% @doc Return the show attribute of the presence.

-spec get_show
(exmpp_xml:xmlel()) -> presenceshow() | undefined.

get_show(#xmlel{ns = NS} = Presence) when ?IS_PRESENCE(Presence) ->
    case exmpp_xml:get_element(Presence, NS, 'show') of
        undefined ->
            'online';
        Show_El ->
            case exmpp_utils:strip(exmpp_xml:get_cdata(Show_El)) of
                <<"away">> -> 'away';
                <<"chat">> -> 'chat';
                <<"dnd">>  -> 'dnd';
                <<"xa">>   -> 'xa';
                _          -> undefined
            end
    end.

%% @spec (Presence, Show) -> New_Presence
%%     Presence = exmpp_xml:xmlel()
%%     Show = online | away | chat | dnd | xa | binary() | string()
%%     New_Presence = exmpp_xml:xmlel()
%% @doc Set the `<show/>' field of a presence stanza.
%%
%% If `Type' is an empty string or the atom `online', the `<show/>'
%% element is removed.

-spec set_show
(exmpp_xml:xmlel(), presenceshow() | binary() | string()) -> exmpp_xml:xmlel().

set_show(#xmlel{ns = NS} = Presence, <<>>) when ?IS_PRESENCE(Presence)->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, "") when ?IS_PRESENCE(Presence)->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, 'online')
  when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, <<"online">>)
  when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'show');
set_show(#xmlel{ns = NS} = Presence, "online")
  when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'show');

set_show(Presence, Show) when is_binary(Show) ->
    set_show(Presence, binary_to_list(Show));
set_show(Presence, Show) when is_list(Show) ->
    set_show(Presence, list_to_atom(Show));

set_show(#xmlel{ns = NS} = Presence, Show)
  when ?IS_PRESENCE(Presence), is_atom(Show) ->
    Show_B = case Show of
		 'away' -> <<"away">>;
		 'chat' -> <<"chat">>;
		 'dnd'  -> <<"dnd">>;
		 'xa'   -> <<"xa">>;
		 _      -> throw({presence, set_show, invalid_show, Show})
	     end,
    New_Show_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'show'}, Show_B),
    case exmpp_xml:get_element(Presence, NS, 'show') of
        undefined ->
            exmpp_xml:append_child(Presence, New_Show_El);
        Show_El ->
            exmpp_xml:replace_child(Presence, Show_El, New_Show_El)
    end.

%% @spec (Presence) -> Status | undefined
%%     Presence = exmpp_xml:xmlel()
%%     Status = binary()
%% @doc Return the status attribute of the presence.

-spec get_status
(exmpp_xml:xmlel()) -> binary() | undefined.

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
%%
%% If `Status' is an empty string (or an empty binary), the previous
%% status is removed.

-spec set_status
(exmpp_xml:xmlel(), binary() | string() | undefined) -> exmpp_xml:xmlel().

set_status(#xmlel{ns = NS} = Presence, undefined)
  when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'status');
set_status(#xmlel{ns = NS} = Presence, <<>>) when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'status');
set_status(#xmlel{ns = NS} = Presence, "") when ?IS_PRESENCE(Presence) ->
    exmpp_xml:remove_element(Presence, NS, 'status');
set_status(#xmlel{ns = NS} = Presence, Status) when ?IS_PRESENCE(Presence) ->
    New_Status_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'status'},
					Status),
    case exmpp_xml:get_element(Presence, NS, 'status') of
        undefined ->
            exmpp_xml:append_child(Presence, New_Status_El);
        Status_El ->
            exmpp_xml:replace_child(Presence, Status_El, New_Status_El)
    end.

%% @spec (Presence) -> Priority
%%     Presence = exmpp_xml:xmlel()
%%     Priority = integer()
%% @doc Return the priority attribute of the presence.

-spec get_priority
(exmpp_xml:xmlel()) -> integer().

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

-spec set_priority
(exmpp_xml:xmlel(), integer()) -> exmpp_xml:xmlel().

set_priority(#xmlel{ns = NS} = Presence, Priority)
  when ?IS_PRESENCE(Presence) andalso is_integer(Priority) ->
    New_Priority_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'priority'},
					  Priority),
    case exmpp_xml:get_element(Presence, NS, 'priority') of
        undefined ->
            exmpp_xml:append_child(Presence, New_Priority_El);
        Priority_El ->
            exmpp_xml:replace_child(Presence, Priority_El, New_Priority_El)
    end.
