% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>
%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>
%% @copyright Process-One [http://www.process-one.net/]

%% @doc
%% The module <strong>{@module}</strong> provides helpers to manipulate
%% standard stanza.

-module(exmpp_stanza).
-vsn('$Revision$').

-include("exmpp.hrl").

% Stanza common components.
-export([
  get_error/1
]).

% Stanza standard attributes.
-export([
  get_sender/1,
  get_sender_from_attrs/1,
  set_sender/2,
  set_sender_in_attrs/2,
  remove_sender/1,
  remove_sender_in_attrs/1,
  get_recipient/1,
  get_recipient_from_attrs/1,
  set_recipient/2,
  set_recipient_in_attrs/2,
  remove_recipient/1,
  remove_recipient_in_attrs/1,
  set_jids_in_attrs/3,
  set_jids/3,
  get_id/1,
  get_id_from_attrs/1,
  set_id/2,
  set_id_in_attrs/2,
  get_type/1,
  get_type_from_attrs/1,
  set_type/2,
  set_type_in_attrs/2,
  get_lang/1,
  get_lang_from_attrs/1,
  set_lang/2,
  set_lang_in_attrs/2
]).

% Common operations.
-export([
  reply/1,
  reply_without_content/1,
  reply_from_attrs/1,
  reply_with_error/2
]).

% Stanza-level errors.
-export([
  error/2,
  error/3,
  stanza_error/2,
  stanza_error_without_original/2,
  is_stanza_error/1,
  get_error_type/1,
  set_error_type/2,
  set_error_type_from_condition/2,
  get_condition/1,
  get_text/1
]).

% Serialization wrappers.
-export([
  to_list/2,
  to_list/3,
  to_list/1,
  to_binary/2,
  to_binary/3,
  to_binary/1,
  to_iolist/2,
  to_iolist/3,
  to_iolist/1
]).

% --------------------------------------------------------------------
% Stanza common components.
% --------------------------------------------------------------------

%% @spec (Stanza) -> Error | undefined
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Error = exmpp_xml:xmlel()
%% @doc Return the error element from `Stanza'.
%%
%% The error element is supposed to have the name `error' and the same
%% namespace as the stanza.

get_error(#xmlel{ns = NS} = Stanza) ->
    exmpp_xml:get_element(Stanza, NS, 'error');
get_error(#iq{type = error, error = Error}) ->
    Error;
get_error(#iq{}) ->
    undefined.

% --------------------------------------------------------------------
% Stanza standard attributes.
% --------------------------------------------------------------------

%% @spec (Stanza) -> Sender | undefined
%%     Stanza = exmpp_xml:xmlel()
%%     Sender = string()
%% @doc Return the sender.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:list_to_jid/1}.

get_sender(#xmlel{attrs = Attrs} = _Stanza) ->
    get_sender_from_attrs(Attrs).

%% @spec (Attrs) -> Sender | undefined
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Sender = string()
%% @doc Return the sender.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:list_to_jid/1}.

get_sender_from_attrs(Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, 'from', undefined).

%% @spec (Stanza, Sender) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     Sender = exmpp_jid:jid() | string() | undefined
%%     New_Stanza = exmpp_xml:xmlel()
%% @doc Set the sender.
%%
%% If `Sender' is set to `undefined', the sender is removed.

set_sender(#xmlel{attrs = Attrs} = Stanza, Sender) ->
    New_Attrs = set_sender_in_attrs(Attrs, Sender),
    Stanza#xmlel{attrs = New_Attrs}.

%% @spec (Attrs, Sender) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Sender = exmpp_jid:jid() | string() | undefined
%%     New_Attrs = [exmpp_xml:xmlnsattribute()]
%% @doc Set the sender.
%%
%% If `Sender' is set to `undefined', the sender is removed.

set_sender_in_attrs(Attrs, undefined) ->
    remove_sender_in_attrs(Attrs);
set_sender_in_attrs(Attrs, Sender) when ?IS_JID(Sender) ->
    set_sender_in_attrs(Attrs, exmpp_jid:jid_to_list(Sender));
set_sender_in_attrs(Attrs, Sender) ->
    exmpp_xml:set_attribute_in_list(Attrs, 'from', Sender).

%% @spec (Stanza) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     New_Stanza = exmpp_xml:xmlel()
%% @doc Remove the sender.

remove_sender(#xmlel{attrs= Attrs} = Stanza) ->
    New_Attrs = remove_sender_in_attrs(Attrs),
    Stanza#xmlel{attrs = New_Attrs}.

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     New_Attrs = [exmpp_xml:xmlnattribute()]
%% @doc Remove the sender.

remove_sender_in_attrs(Attrs) ->
    exmpp_xml:remove_attribute_from_list(Attrs, 'from').

%% @spec (Stanza) -> Recipient | undefined
%%     Stanza = exmpp_xml:xmlel()
%%     Recipient = string()
%% @doc Return the recipient.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:list_to_jid/1}.

get_recipient(#xmlel{attrs = Attrs} = _Stanza) ->
    get_recipient_from_attrs(Attrs).

%% @spec (Attrs) -> Recipient | undefined
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Recipient = string()
%% @doc Return the recipient.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:list_to_jid/1}.

get_recipient_from_attrs(Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, 'to', undefined).

%% @spec (Stanza, Recipient) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     Recipient = exmpp_jid:jid() | string() | undefined
%%     New_Stanza = exmpp_xml:xmlel()
%% @doc Set the recipient.
%%
%% If `Recipient' is set to `undefined', the recipient is removed.

set_recipient(#xmlel{attrs = Attrs} = Stanza, Recipient) ->
    New_Attrs = set_recipient_in_attrs(Attrs, Recipient),
    Stanza#xmlel{attrs = New_Attrs}.

%% @spec (Attrs, Recipient) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Recipient = exmpp_jid:jid() | string() | undefined
%%     New_Attrs = [exmpp_xml:xmlnattribute()]
%% @doc Set the recipient.
%%
%% If `Recipient' is set to `undefined', the recipient is removed.

set_recipient_in_attrs(Attrs, undefined) ->
    remove_recipient_in_attrs(Attrs);
set_recipient_in_attrs(Attrs, Recipient) when ?IS_JID(Recipient) ->
    set_recipient_in_attrs(Attrs, exmpp_jid:jid_to_list(Recipient));
set_recipient_in_attrs(Attrs, Recipient) ->
    exmpp_xml:set_attribute_in_list(Attrs, 'to', Recipient).

%% @spec (Stanza) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     New_Stanza = exmpp_xml:xmlel()
%% @doc Remove the recipient.

remove_recipient(#xmlel{attrs= Attrs} = Stanza) ->
    New_Attrs = remove_recipient_in_attrs(Attrs),
    Stanza#xmlel{attrs = New_Attrs}.

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     New_Attrs = [exmpp_xml:xmlnattribute()]
%% @doc Remove the recipient.

remove_recipient_in_attrs(Attrs) ->
    exmpp_xml:remove_attribute_from_list(Attrs, 'to').

%% @spec (Stanza, Sender, Recipient) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     Sender = exmpp_jid:jid() | string() | undefined
%%     Recipient = exmpp_jid:jid() | string() | undefined
%%     New_Stanza = exmpp_xml:xmlel()
%% @doc Set the sender and the recipient at the same time.
%%
%% If `Sender' is set to `undefined', the sender is removed. If
%% `Recipient' is set to `undefined', the recipient is removed.

set_jids(Stanza, From, To) ->
    set_recipient(set_sender(Stanza, From), To).

%% @spec (Attrs, Sender, Recipient) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Sender = exmpp_jid:jid() | string() | undefined
%%     Recipient = exmpp_jid:jid() | string() | undefined
%%     New_Attrs = [exmpp_xml:xmlnattribute()]
%% @doc Set the sender and the recipient at the same time.
%%
%% If `Sender' is set to `undefined', the sender is removed. If
%% `Recipient' is set to `undefined', the recipient is removed.

set_jids_in_attrs(Attrs, From, To) ->
    set_recipient_in_attrs(set_sender_in_attrs(Attrs, From), To).

%% @spec (Stanza) -> ID | undefined
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     ID = string()
%% @doc Return the stanza ID.

get_id(#xmlel{attrs = Attrs} = _Stanza) ->
    get_id_from_attrs(Attrs);
get_id(#iq{id = ID}) ->
    ID.

%% @spec (Attrs) -> ID | undefined
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     ID = string()
%% @doc Return the stanza ID.

get_id_from_attrs(Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, 'id', undefined).

%% @spec (Stanza, ID) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     ID = string() | undefined
%%     New_Stanza = exmpp_xml:xmlel() | iq()
%% @doc Set the id.

set_id(#xmlel{attrs = Attrs, name = Name} = Stanza, ID)
  when ID == undefined; ID == "" ->
    New_Attrs = set_id_in_attrs(Attrs, exmpp_utils:random_id(Name)),
    Stanza#xmlel{attrs = New_Attrs};
set_id(#xmlel{attrs = Attrs} = Stanza, ID) ->
    New_Attrs = set_id_in_attrs(Attrs, ID),
    Stanza#xmlel{attrs = New_Attrs};
set_id(#iq{} = Stanza, ID)
  when ID == undefined; ID == "" ->
    Stanza#iq{id = exmpp_utils:random_id("iq")};
set_id(#iq{} = Stanza, ID) ->
    Stanza#iq{id = ID}.

%% @spec (Attrs, ID) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     ID = string()
%%     New_Attrs = [exmpp_xml:xmlnattribute()]
%% @doc Set the id.

set_id_in_attrs(Attrs, ID) when ID == undefined; ID == "" ->
    set_id_in_attrs(Attrs, exmpp_utils:random_id("stanza"));
set_id_in_attrs(Attrs, ID) ->
    exmpp_xml:set_attribute_in_list(Attrs, 'id', ID).

%% @spec (Stanza) -> Type | undefined
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Type = string()
%% @doc Return the type of the stanza.

get_type(#xmlel{attrs = Attrs} = _Stanza) ->
    get_type_from_attrs(Attrs);
get_type(#iq{type = Type}) ->
    atom_to_list(Type).

%% @spec (Attrs) -> Type | undefined
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Type = string()
%% @doc Return the type of the stanza.

get_type_from_attrs(Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, 'type', undefined).

%% @spec (Stanza, Type) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Type = atom() | string()
%%     New_Stanza = exmpp_xml:xmlel() | iq()
%% @doc Set the type of the stanza.

set_type(#xmlel{attrs = Attrs} = Stanza, Type) ->
    New_Attrs = set_type_in_attrs(Attrs, Type),
    Stanza#xmlel{attrs = New_Attrs};
set_type(#iq{} = Stanza, 'get') ->
    Stanza#iq{type = get, kind = request};
set_type(#iq{} = Stanza, 'set') ->
    Stanza#iq{type = set, kind = request};
set_type(#iq{} = Stanza, 'result') ->
    Stanza#iq{type = result, kind = response};
set_type(#iq{} = Stanza, 'error') ->
    Stanza#iq{type = error, kind = response};
set_type(#iq{} = Stanza, Type) when is_list(Type) ->
    set_type(Stanza, list_to_atom(Type)).

%% @spec (Attrs, Type) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Type = atom() | string()
%%     New_Attrs = [exmpp_xml:xmlnsattribute()]
%% @doc Set the type of the stanza.

set_type_in_attrs(Attrs, Type) when is_atom(Type) ->
    set_type_in_attrs(Attrs, atom_to_list(Type));
set_type_in_attrs(Attrs, Type) ->
    exmpp_xml:set_attribute_in_list(Attrs, 'type', Type).

%% @spec (Stanza) -> Lang | undefined
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Lang = string()
%% @doc Return the language of the stanza.

get_lang(#xmlel{attrs = Attrs} = _Stanza) ->
    get_lang_from_attrs(Attrs);
get_lang(#iq{lang = Lang}) ->
    Lang.

%% @spec (Attrs) -> Lang | undefined
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Lang = string()
%% @doc Return the language of the stanza.

get_lang_from_attrs(Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, ?NS_XML, 'lang', undefined).

%% @spec (Stanza, Lang) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel() | iq()
%%     Lang = string()
%%     New_Stanza = exmpp_xml:xmlel() | iq()
%% @doc Set the lang.

set_lang(#xmlel{attrs = Attrs} = Stanza, Lang) ->
    New_Attrs = set_lang_in_attrs(Attrs, Lang),
    Stanza#xmlel{attrs = New_Attrs};
set_lang(#iq{} = Stanza, Lang) ->
    Stanza#iq{lang = Lang}.

%% @spec (Attrs, Lang) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     Lang = string()
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%% @doc Set the lang.

set_lang_in_attrs(Attrs, Lang) ->
    exmpp_xml:set_attribute_in_list(Attrs, ?NS_XML, 'lang', Lang).

% --------------------------------------------------------------------
% Common operations.
% --------------------------------------------------------------------

%% @spec (Stanza) -> Stanza_Reply
%%     Stanza = exmpp_xml:xmlel()
%%     Stanza_Reply = exmpp_xml:xmlel()
%% @doc Prepare a reply to `Stanza'.
%%
%% @see reply_from_attrs/1.

reply(#xmlel{attrs = Attrs} = Stanza) ->
    New_Attrs = reply_from_attrs(Attrs),
    Stanza#xmlel{attrs = New_Attrs}.

%% @spec (Stanza) -> Stanza_Reply
%%     Stanza = exmpp_xml:xmlel()
%%     Stanza_Reply = exmpp_xml:xmlel()
%% @doc Prepare a reply to `Stanza' with children removed.
%%
%% @see reply_from_attrs/1.

reply_without_content(#xmlel{attrs = Attrs} = Stanza) ->
    New_Attrs = reply_from_attrs(Attrs),
    Stanza#xmlel{attrs = New_Attrs, children = []}.

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute()]
%%     New_Attrs = [exmpp_xml:xmlnsattribute()]
%% @doc Handles `to' and `from' attributes to prepare a reply stanza.

reply_from_attrs(Attrs) ->
    Sender = get_sender_from_attrs(Attrs),
    Recipient = get_recipient_from_attrs(Attrs),
    set_jids_in_attrs(Attrs, Recipient, Sender).

%% @spec (Stanza, Error) -> Stanza_Reply
%%     Stanza = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel() | atom()
%%     Stanza_Reply = exmpp_xml:xmlel()
%% @doc Prepare an error reply to `Stanza'.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

reply_with_error(Stanza, Condition) when is_atom(Condition) ->
    Error = error(Stanza#xmlel.ns, Condition),
    reply_with_error(Stanza, Error);
reply_with_error(Stanza, Error) ->
    Reply = reply(Stanza),
    stanza_error(Reply, Error).

% --------------------------------------------------------------------
% Stanza-level errors.
% --------------------------------------------------------------------

standard_conditions() ->
    [
      {'bad-request',             "modify" },
      {'conflict',                "cancel" },
      {'feature-not-implemented', "cancel" },
      {'forbidden',               "auth"   },
      {'gone',                    "modify" },
      {'internal-server-error',   "wait"   },
      {'item-not-found',          "cancel" },
      {'jid-malformed',           "modify" },
      {'not-acceptable',          "modify" },
      {'not-allowed',             "cancel" },
      {'not-authorized',          "auth"   },
      {'payment-required',        "auth"   },
      {'recipient-unavailable',   "wait"   },
      {'redirect',                "modify" },
      {'registration-required',   "auth"   },
      {'remote-server-not-found', "cancel" },
      {'remote-server-timeout',   "wait"   },
      {'resource-constraint',     "wait"   },
      {'service-unavailable',     "cancel" },
      {'subscription-required',   "auth"   },
      {'unexpected-request',      "wait"   },
      {'undefined-condition',     undefined}
    ].

%% @spec (NS, Condition) -> Stanza_Error
%%     NS = atom() | string()
%%     Condition = atom()
%%     Stanza_Error = exmpp_xml:xmlel()
%% @doc Create an `<error/>' element based on the given `Condition'.
%%
%% A default type is set by {@link set_error_type/2} if `NS' is
%% `jabber:client' or `jabber:server'. This does not contain any text
%% element.

error(NS, Condition) ->
    error(NS, Condition, {undefined, undefined}).

%% @spec (NS, Condition, Text_Spec) -> Stanza_Error
%%     NS = atom() | string()
%%     Condition = atom()
%%     Text_Spec = {Lang, Text} | Text | undefined
%%     Lang = string() | undefined
%%     Text = string() | undefined
%%     Stanza_Error = exmpp_xml:xmlel()
%% @doc Create an `<error/>' element based on the given `Condition'.
%%
%% A default type is set by {@link set_error_type/2} if `NS' is
%% `jabber:client' or `jabber:server'. This does not contain any text
%% element.

error(NS, Condition, {Lang, Text}) ->
    Condition_El = #xmlel{
      ns = ?NS_STANZA_ERRORS,
      name = Condition
    },
    Error_El0 = #xmlel{
      ns = NS,
      name = 'error',
      children = [Condition_El]
    },
    Error_El = case Text of
        undefined ->
            Error_El0;
        _ ->
            Text_El0 = #xmlel{
              ns = ?NS_STANZA_ERRORS,
              name = 'text'
            },
            Text_El = case Lang of
                undefined ->
                    Text_El0;
                _ ->
                    exmpp_xml:set_attribute(Text_El0, ?NS_XML, 'lang', Lang)
            end,
            exmpp_xml:append_child(Error_El0, Text_El)
    end,
    set_error_type_from_condition_in_error(Error_El, Condition);
error(NS, Condition, Text) ->
    error(NS, Condition, {undefined, Text}).

%% @spec (Stanza, Error) -> Stanza_Error
%%     Stanza = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel()
%%     Stanza_Error = exmpp_xml:xmlel()
%% @doc Transform `Stanza' in a stanza error.
%%
%% The `type' attribute is set and an error condition is added. The
%% caller is still responsible to set or modify the `to' attribute
%% correctly.
%%
%% @see error/2.
%% @see error/3.

stanza_error(Stanza, Error) ->
    Stanza_Error = exmpp_xml:append_child(Stanza, Error),
    set_type(Stanza_Error, "error").

%% @spec (Stanza, Error) -> Stanza_Error
%%     Stanza = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel()
%%     Stanza_Error = exmpp_xml:xmlel()
%% @doc Transform `Stanza' in a stanza error.
%%
%% Previous child elements from `Stanza' are not kept.
%%
%% @see stanza_error/2.

stanza_error_without_original(Stanza, Error) ->
    Stanza_Error = exmpp_xml:set_children(Stanza, [Error]),
    set_type(Stanza_Error, "error").

%% @spec (Stanza) -> bool()
%%     Stanza = exmpp_xml:xmlel()
%% @doc Tell if the stanza transports an error.

is_stanza_error(Stanza) ->
    case get_type(Stanza) of
        "error" -> true;
        _       -> false
    end.

%% @spec (Stanza) -> Type
%%     Stanza = exmpp_xml:xmlel()
%%     Type = string()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Return the type of the error element.

get_error_type(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            get_error_type_from_error(Error)
    end.

get_error_type_from_error(Error) ->
    exmpp_xml:get_attribute(Error, 'type', "").

%% @spec (Stanza, Type) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     Type = string()
%%     New_Stanza = exmpp_xml:xmlel()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Set the type of the error element.

set_error_type(Stanza, Type) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            New_Error = set_error_type_in_error(Error, Type),
            exmpp_xml:replace_child(Stanza, Error, New_Error)
    end.

set_error_type_in_error(Error, Type) ->
    exmpp_xml:set_attribute(Error, 'type', Type).

%% @spec (Stanza, Condition) -> New_Stanza
%%     Stanza = exmpp_xml:xmlel()
%%     Condition = atom()
%%     New_Stanza = exmpp_xml:xmlel()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza} |
%%         {stanza_error, error_type, invalid_condition, {NS, Condition}}
%% @doc Set the type of the error element, based on the given condition.
%%
%% If the condition is `undefined-condition', the type is unchanged.

set_error_type_from_condition(Stanza, Condition) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            New_Error = set_error_type_from_condition_in_error(Error,
              Condition),
            exmpp_xml:replace_child(Stanza, Error, New_Error)
    end.

set_error_type_from_condition_in_error(#xmlel{ns = NS} = Error,
  Condition) when NS == ?NS_JABBER_CLIENT; NS == ?NS_JABBER_SERVER ->
    case lists:keysearch(Condition, 1, standard_conditions()) of
        {value, {_, undefined}} ->
            Error;
        {value, {_, Type}} ->
            set_error_type_in_error(Error, Type);
        false ->
            throw({stanza_error, error_type, invalid_condition,
                {NS, Condition}})
    end;
set_error_type_from_condition_in_error(Error, _Condition) ->
    Error.

%% @spec (Stanza) -> Condition | undefined
%%     Stanza = exmpp_xml:xmlel()
%%     Condition = atom()
%% @throws {stanza_error, condition, no_error_element_found, Stanza} |
%%         {stanza_error, condition, no_condition_found, Error}
%% @doc Return the child element name corresponding to the stanza error
%% condition.
%%
%% If the namespace isn't neither `jabber:client' nor `jabber:server',
%% the name of the first child is returned.

get_condition(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, condition, no_error_element_found, Stanza});
        Error ->
            get_condition_in_error(Error)
    end.

get_condition_in_error(#xmlel{ns = NS} = Error)
  when NS == ?NS_JABBER_CLIENT; NS == ?NS_JABBER_SERVER ->
    case exmpp_xml:get_element_by_ns(Error, ?NS_STANZA_ERRORS) of
        undefined ->
            % This <error/> element is invalid because the condition must be
            % present (and first).
            throw({stanza_error, condition, no_condition_found, Error});
        #xmlel{name = 'text'} ->
            % Same as above.
            throw({stanza_error, condition, no_condition_found, Error});
        #xmlel{name = Condition} ->
            Condition
    end;
get_condition_in_error(#xmlel{children = [First | _]} = _Error) ->
    First#xmlel.name;
get_condition_in_error(_Error) ->
    undefined.

%% @spec (Stanza) -> Text | undefined
%%     Stanza = exmpp_xml:xmlel()
%%     Text = string()
%% @throws {stanza_error, text, no_error_element_found, Stanza}
%% @doc Return the text that describes the error.
%%
%% If there is no `<text/>' element, an empty string is returned.

get_text(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, text, no_error_element_found, Stanza});
        Error ->
            get_text_in_error(Error)
    end.

get_text_in_error(#xmlel{ns = NS} = Error)
  when NS == ?NS_JABBER_CLIENT; NS == ?NS_JABBER_SERVER ->
    case exmpp_xml:get_element(Error, ?NS_STANZA_ERRORS, 'text') of
        undefined -> undefined;
        Text      -> exmpp_xml:get_cdata_as_list(Text)
    end;
get_text_in_error(Error) ->
    case exmpp_xml:get_element(Error, 'text') of
        undefined -> undefined;
        Text      -> exmpp_xml:get_cdata_as_list(Text)
    end.

% --------------------------------------------------------------------
% Serialization wrappers.
% --------------------------------------------------------------------

%% @spec (El, Default_NS) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | Equivalent_NSs
%%     NS = atom() | string()
%%     Equivalent_NSs = [NS]
%%     XML_Text = string()
%% @doc Serialize a stanza using the given default namespace.
%%
%% The XMPP namespace `http://etherx.jabber.org/streams' and the
%% Server Dialback `jabber:server:dialback' are included as a prefixed
%% namespace, with the `stream' prefix.

to_list(#iq{} = El, Default_NS) ->
    to_list(exmpp_iq:iq_to_xmlel(El), Default_NS);
to_list(El, Default_NS) ->
    to_list(El, Default_NS,
      [{?NS_XMPP, ?NS_XMPP_pfx}, {?NS_DIALBACK, ?NS_DIALBACK_pfx}]).

%% @spec (El, Default_NS, Prefix) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_Text = string()
%% @doc Serialize a stanza using the given namespaces.
%%
%% To understand `Default_NS', see {@link exmpp_xml:xmlel_to_xmlelement/3}.

to_list(#iq{} = El, Default_NS, Prefixed_NS) ->
    to_list(exmpp_iq:iq_to_xmlel(El), Default_NS, Prefixed_NS);
to_list(El, Default_NS, Prefixed_NS) ->
    exmpp_xml:node_to_list(El, [Default_NS], Prefixed_NS).

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     XML_Text = string()
%% @doc Serialize a stanza using common XMPP default namespaces.
%%
%% This function calls {@link to_list/2} with `Default_NS' set to
%% `[?NS_JABBER_CLIENT, ?NS_JABBER_SERVER, ?NS_COMPONENT_ACCEPT,
%% ?NS_COMPONENT_CONNECT]'.

to_list(#iq{} = El) ->
    to_list(exmpp_iq:iq_to_xmlel(El));
to_list(El) ->
    to_list(El, [
        ?NS_JABBER_CLIENT,
        ?NS_JABBER_SERVER,
        ?NS_COMPONENT_ACCEPT,
        ?NS_COMPONENT_CONNECT
      ]).

%% @spec (El, Default_NS) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | Equivalent_NSs
%%     NS = atom() | string()
%%     Equivalent_NSs = [NS]
%%     XML_Text = binary()
%% @doc Serialize a stanza using the given default namespace.
%%
%% The XMPP namespace `http://etherx.jabber.org/streams' and the
%% Server Dialback `jabber:server:dialback' are included as a prefixed
%% namespace, with the `stream' prefix.

to_binary(#iq{} = El, Default_NS) ->
    to_binary(exmpp_iq:iq_to_xmlel(El), Default_NS);
to_binary(El, Default_NS) ->
    to_binary(El, Default_NS,
      [{?NS_XMPP, ?NS_XMPP_pfx}, {?NS_DIALBACK, ?NS_DIALBACK_pfx}]).

%% @spec (El, Default_NS, Prefix) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_Text = binary()
%% @doc Serialize a stanza using the given namespaces.
%%
%% To understand `Default_NS', see {@link exmpp_xml:xmlel_to_xmlelement/3}.

to_binary(#iq{} = El, Default_NS, Prefixed_NS) ->
    to_binary(exmpp_iq:iq_to_xmlel(El), Default_NS, Prefixed_NS);
to_binary(El, Default_NS, Prefixed_NS) ->
    exmpp_xml:node_to_binary(El, [Default_NS], Prefixed_NS).

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     XML_Text = binary()
%% @doc Serialize a stanza using common XMPP default namespaces.
%%
%% This function calls {@link to_binary/2} with `Default_NS' set to
%% `[?NS_JABBER_CLIENT, ?NS_JABBER_SERVER, ?NS_COMPONENT_ACCEPT,
%% ?NS_COMPONENT_CONNECT]'.

to_binary(#iq{} = El) ->
    to_binary(exmpp_iq:iq_to_xmlel(El));
to_binary(El) ->
    to_binary(El, [
        ?NS_JABBER_CLIENT,
        ?NS_JABBER_SERVER,
        ?NS_COMPONENT_ACCEPT,
        ?NS_COMPONENT_CONNECT
      ]).

%% @spec (El, Default_NS) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | Equivalent_NSs
%%     NS = atom() | string()
%%     Equivalent_NSs = [NS]
%%     XML_Text = iolist()
%% @doc Serialize a stanza using the given default namespace.
%%
%% The XMPP namespace `http://etherx.jabber.org/streams' and the
%% Server Dialback `jabber:server:dialback' are included as a prefixed
%% namespace, with the `stream' prefix.

to_iolist(#iq{} = El, Default_NS) ->
    to_iolist(exmpp_iq:iq_to_xmlel(El), Default_NS);
to_iolist(El, Default_NS) ->
    to_iolist(El, Default_NS,
      [{?NS_XMPP, ?NS_XMPP_pfx}, {?NS_DIALBACK, ?NS_DIALBACK_pfx}]).

%% @spec (El, Default_NS, Prefix) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     Default_NS = NS | [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_Text = iolist()
%% @doc Serialize a stanza using the given namespaces.
%%
%% To understand `Default_NS', see {@link exmpp_xml:xmlel_to_xmlelement/3}.

to_iolist(#iq{} = El, Default_NS, Prefixed_NS) ->
    to_iolist(exmpp_iq:iq_to_xmlel(El), Default_NS, Prefixed_NS);
to_iolist(El, Default_NS, Prefixed_NS) ->
    exmpp_xml:node_to_iolist(El, [Default_NS], Prefixed_NS).

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | iq() | list()
%%     XML_Text = iolist()
%% @doc Serialize a stanza using common XMPP default namespaces.
%%
%% This function calls {@link to_iolist/2} with `Default_NS' set to
%% `[?NS_JABBER_CLIENT, ?NS_JABBER_SERVER, ?NS_COMPONENT_ACCEPT,
%% ?NS_COMPONENT_CONNECT]'.

to_iolist(#iq{} = El) ->
    to_iolist(exmpp_iq:iq_to_xmlel(El));
to_iolist(El) ->
    to_iolist(El, [
        ?NS_JABBER_CLIENT,
        ?NS_JABBER_SERVER,
        ?NS_COMPONENT_ACCEPT,
        ?NS_COMPONENT_CONNECT
      ]).
