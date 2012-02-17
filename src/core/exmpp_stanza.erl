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
%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helpers to manipulate
%% standard stanza.

-module(exmpp_stanza).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Stanza common components.
-export([
     get_error/1
]).

%% Stanza standard attributes.
-export([
    get_sender/1,
    set_sender/2,
    remove_sender/1,
    get_recipient/1,
    set_recipient/2,
    remove_recipient/1,
    set_jids/3,
    get_id/1,
    set_id/2,
    get_type/1,
    set_type/2,
    get_lang/1,
    set_lang/2
]).

%% Common operations.
-export([
    reply/1,
    reply_without_content/1,
    reply_with_error/2
]).

%% Stanza-level errors.
-export([
    error/1,
    error/2,
    stanza_error/2,
    stanza_error_without_original/2,
    is_stanza_error/1,
    get_error_type/1,
    set_error_type/2,
    set_error_type_from_condition/2,
    get_condition/1,
    get_text/1
]).

%% Serialization wrappers.
-export([
    to_iolist/1
]).

-define(Is_IQ_Type_Request(Type),
(
  Type == <<"get">> orelse
  Type == <<"set">>
)).

-define(Is_IQ_Type_Response(Type),
(
  Type == <<"result">> orelse
  Type == <<"error">>
)).

%%
-define(Is_IQ_Type(Type),
(
  Type == <<"get">>    orelse
  Type == <<"set">>    orelse
  Type == <<"result">> orelse
  Type == <<"error">>
)).

%%
-define(Is_Type(Type),
(
  %% IQ Type
  Type == <<"get">>          orelse
  Type == <<"set">>          orelse
  Type == <<"result">>       orelse
  %% Message Type
  Type == <<"normal">>       orelse
  Type == <<"headline">>     orelse
  Type == <<"chat">>         orelse
  Type == <<"groupchat">>    orelse
  %% Presence Type
  Type == <<"unavailable">>  orelse
  Type == <<"subscribe">>    orelse
  Type == <<"subscribed">>   orelse
  Type == <<"unsubscribe">>  orelse
  Type == <<"unsubscribed">> orelse
  Type == <<"probe">> orelse
  %% Common Type
  Type == <<"error">>
)).




-define(Is_Xmlel_Error(Xmlel_Error),
(
  Xmlel_Error#xmlel.name == <<"error">>
)).

%% --------------------------------------------------------------------
%% Documentation / type definition.
%% --------------------------------------------------------------------

-export_type([
  standard_condition/0,
  standard_conditions/0,
  error_condition/0,
  error_type/0,
  error_text/0,
  error_lang/0
]).

-type(error_condition() :: binary()).
-type(error_type()      :: binary()).
-type(error_lang()      :: binary()).
-type(error_text()      :: binary()).

-type(standard_condition()
  :: {Error_Condition :: exmpp_stanza:error_condition(),
      Error_Type      :: exmpp_stanza:error_type() | undefined}

).

-type(standard_conditions()
  :: [Standard_Condition::exmpp_stanza:standard_condition(),...]
).

%%
-export_type([
  id/0,
  to/0,
  from/0,
  lang/0
]).

-type(id() :: binary()).
-type(from() :: binary()).
-type(to()   :: binary()).
-type(lang() :: binary()).

-export_type([
  type/0,
  %%
  type_get/0,
  type_set/0,
  type_result/0,
  %%
  type_chat/0,
  type_groupchat/0,
  type_headline/0,
  type_normal/0,
  %%
  type_unavailable/0,
  type_subscribe/0,
  type_unsubscribe/0,
  type_subscribed/0,
  type_unsubscribed/0,
  type_probe/0,
  %%
  type_error/0
]).

%% IQ Type
-type(type_get()    :: <<_:24>>).
-type(type_set()    :: <<_:24>>).
-type(type_result() :: <<_:48>>).
%% Message Type
-type(type_chat()      :: <<_:32>>).
-type(type_groupchat() :: <<_:72>>).
-type(type_headline()  :: <<_:64>>).
-type(type_normal()    :: <<_:48>>).
%% Presence Type
-type(type_subscribe()    :: <<_:72>>).
-type(type_subscribed()   :: <<_:80>>).
-type(type_unavailable()  :: <<_:88>>).
-type(type_unsubscribe()  :: <<_:88>>).
-type(type_unsubscribed() :: <<_:96>>).
-type(type_probe() :: <<_:40>>).
%% Common Type
-type(type_error() :: <<_:40>>).

-type(type() :: exmpp_stanza:type_get()
              | exmpp_stanza:type_set()
              | exmpp_stanza:type_result()
              | exmpp_stanza:type_chat()
              | exmpp_stanza:type_groupchat()
              | exmpp_stanza:type_headline()
              | exmpp_stanza:type_normal()
              | exmpp_stanza:type_subscribe()
              | exmpp_stanza:type_subscribed()
              | exmpp_stanza:type_unsubscribed()
              | exmpp_stanza:type_unsubscribed()
              | exmpp_stanza:type_probe()
              | exmpp_stanza:type_error()
).


-export_type([
  stanza_type/0,
  iq_type/0,
  message_type/0,
  presence_type/0
]).

-type(iq_type() :: exmpp_stanza:type_get()
                 | exmpp_stanza:type_set()
                 | exmpp_stanza:type_result()
                 | exmpp_stanza:type_error()
).

-type(message_type() :: exmpp_stanza:type_chat()
                      | exmpp_stanza:type_groupchat()
                      | exmpp_stanza:type_headline()
                      | exmpp_stanza:type_normal()
                      | exmpp_stanza:type_error()
).

-type(presence_type() :: exmpp_stanza:type_unavailable()
                       | exmpp_stanza:type_subscribe()
                       | exmpp_stanza:type_subscribed()
                       | exmpp_stanza:type_unsubscribe()
                       | exmpp_stanza:type_unsubscribed()
                       | exmpp_stanza:type_probe()
                       | exmpp_stanza:type_error()
).

-type(stanza_type() :: exmpp_stanza:iq_type()
                     | exmpp_stanza:message_type()
                     | exmpp_stanza:presence_type()
).


-export_type([
  stanza/0,
  %%
  iq/0,
  iq_get/0,
  iq_set/0,
  iq_result/0,
  iq_error/0,
  %%
  message/0,
  message_normal/0,
  message_chat/0,
  message_headline/0,
  message_groupchat/0,
  message_error/0,
  %%
  presence/0,
  presence_available/0,
  presence_unavailable/0,
  presence_probe/0,
  presence_subscribe/0,
  presence_subscribed/0,
  presence_unsubscribe/0,
  presence_unsubscribed/0,
  presence_error/0
]).

-type(iq_get()
  :: #xmlel{
         name     :: <<_:16>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_get()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}       |
             {From :: <<_:32>>, exmpp_stanza:from()}     |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(iq_set()
  :: #xmlel{
         name     :: <<_:16>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_set()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}       |
             {From :: <<_:32>>, exmpp_stanza:from()}     |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(iq_result()
  :: #xmlel{
         name     :: <<_:16>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_result()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}          |
             {From :: <<_:32>>, exmpp_stanza:from()}        |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(iq_error()
  :: #xmlel{
         name     :: <<_:16>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_error()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}         |
             {From :: <<_:32>>, exmpp_stanza:from()}       |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(iq() :: exmpp_stanza:iq_get()
            | exmpp_stanza:iq_set()
            | exmpp_stanza:iq_result()
            | exmpp_stanza:iq_error()
).

-type(message_normal()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_normal()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}          |
             {From :: <<_:32>>, exmpp_stanza:from()}        |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(message_headline()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_headline()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}            |
             {From :: <<_:32>>, exmpp_stanza:from()}          |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(message_chat()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:message_chat()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}           |
             {From :: <<_:32>>, exmpp_stanza:from()}         |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(message_groupchat()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_groupchat()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}             |
             {From :: <<_:32>>, exmpp_stanza:from()}           |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(message_error()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_error()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}             |
             {From :: <<_:32>>, exmpp_stanza:from()}           |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(message() :: exmpp_stanza:message_normal()
                 | exmpp_stanza:message_chat()
                 | exmpp_stanza:message_headline()
                 | exmpp_stanza:message_groupchat()
                 | exmpp_stanza:message_error()
).

-type(presence_available()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Id   :: <<_:16>>, exmpp_stanza:id()}   |
             {From :: <<_:32>>, exmpp_stanza:from()} |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_unavailable()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_unavailable()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}               |
             {From :: <<_:32>>, exmpp_stanza:from()}             |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_probe()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_probe()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}         |
             {From :: <<_:32>>, exmpp_stanza:from()}       |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_subscribe()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_subscribe()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}             |
             {From :: <<_:32>>, exmpp_stanza:from()}           |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_subscribed()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_subscribed()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}              |
             {From :: <<_:32>>, exmpp_stanza:from()}            |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_unsubscribe()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_unsubscribe()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}               |
             {From :: <<_:32>>, exmpp_stanza:from()}             |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_unsubscribed()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_unsubscribed()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}                |
             {From :: <<_:32>>, exmpp_stanza:from()}              |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence_error()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [
             {Type :: <<_:32>>, exmpp_stanza:type_error()} |
             {Id   :: <<_:16>>, exmpp_stanza:id()}         |
             {From :: <<_:32>>, exmpp_stanza:from()}       |
             {To   :: <<_:16>>, exmpp_stanza:to()}
         ],
         children :: [Xmlel::exxml:el()]
     }
).

-type(presence() :: exmpp_stanza:presence_available()
                  | exmpp_stanza:presence_unavailable()
                  | exmpp_stanza:presence_probe()
                  | exmpp_stanza:presence_subscribe()
                  | exmpp_stanza:presence_subscribed()
                  | exmpp_stanza:presence_unsubscribe()
                  | exmpp_stanza:presence_unsubscribed()
                  | exmpp_stanza:presence_error()
).

-type(stanza() :: exmpp_stanza:iq()
                | exmpp_stanza:message()
                | exmpp_stanza:presence()
).

-export_type([
  xmlel_error/0
]).

-type(xmlel_error()
  :: #xmlel{
         name :: <<_:40>>
     }
).

%% --------------------------------------------------------------------
%% Stanza common components.
%% --------------------------------------------------------------------

%% @doc Return the error element from `Stanza'.
%%
%% The error element is supposed to have the name `error' and the same
%% namespace as the stanza.

-spec(get_error/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Xmlel_Error :: exmpp_stanza:xmlel_error() | undefined
).

get_error(Stanza = #xmlel{}) ->
    exxml:get_el(Stanza, <<"error">>);
get_error(#iq{type = <<"error">>, error = Xmlel_Error}) ->
    Xmlel_Error;
get_error(#iq{}) ->
    undefined.

%% --------------------------------------------------------------------
%% Stanza standard attributes.
%% --------------------------------------------------------------------

%% @doc Return the sender.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:parse/1}.
-spec(get_sender/1 ::
(
  Stanza::exmpp_stanza:stanza())
    -> Sender :: exmpp_stanza:from() | undefined
).

get_sender(Stanza) ->
    exxml:get_attr(Stanza, <<"from">>, undefined).

%% @doc Set the sender.
-spec(set_sender/2 ::
(
  Stanza::exmpp_stanza:stanza(),
  Sender::exmpp_stanza:from())
    -> Stanza::exmpp_stanza:stanza()
).

set_sender(Stanza, Sender) ->
    exxml:set_attr(Stanza, <<"from">>, Sender).

%% @doc Remove the sender.
-spec(remove_sender/1 ::
(
  Stanza::exmpp_stanza:stanza())
    -> Stanza::exmpp_stanza:stanza()
).

remove_sender(Stanza)  ->
    exxml:remove_attr(Stanza, <<"from">>).

%% @doc Return the recipient.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:parse/1}.
-spec(get_recipient/1 ::
(
  Stanza::exmpp_stanza:stanza())
    -> Recipient :: exmpp_stanza:to() | undefined
).

get_recipient(Stanza) ->
    exxml:get_attr(Stanza, <<"to">>, undefined).

%% @doc Set the recipient.
-spec(set_recipient/2 ::
(
  Stanza    :: exmpp_stanza:stanza(),
  Recipient :: exmpp_stanza:to())
    -> Stanza::exmpp_stanza:stanza()
).

set_recipient(Stanza, Recipient) ->
    exxml:set_attr(Stanza, <<"to">>, Recipient).

%% @doc Remove the recipient.
-spec(remove_recipient/1 ::
(
  Stanza::exmpp_stanza:stanza())
    -> Stanza::exmpp_stanza:stanza()
).

remove_recipient(Stanza)  ->
    exxml:remove_attr(Stanza, <<"to">>).

%% @doc Set the sender and the recipient at the same time.
-spec(set_jids/3 ::
(
  Stanza :: exmpp_stanza:stanza(),
  From   :: exmpp_stanza:from(),
  To     :: exmpp_stanza:to())
    -> Stanza::exmpp_stanza:stanza()
).

set_jids(Stanza, From, To)  ->
    set_recipient(set_sender(Stanza, From), To).

%% @doc Return the stanza ID.
-spec(get_id/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Id :: exmpp_stanza:id() | undefined
).

get_id(#iq{id = Id}) ->
    Id;
get_id(Stanza)  ->
    exxml:get_attr(Stanza, <<"id">>, undefined).

%% @doc Set the ID.
-spec(set_id/2 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq(),
  Id     :: exmpp_stanza:id() | 'random')
    -> Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq()
).

set_id(Stanza, 'random') ->
    set_id(Stanza, exmpp_utils:random_id(Stanza#xmlel.name));
set_id(Stanza = #xmlel{}, Id)  ->
    exxml:set_attr(Stanza, <<"id">>, Id);
set_id(IQ, 'random') when is_record(IQ, 'iq')->
    set_id(IQ, exmpp_utils:random_id(<<"iq">>));
set_id(IQ, Id) when is_record(IQ, 'iq') andalso is_binary(Id)->
    IQ#iq{id = Id}.

%% @doc Return the type of the stanza.
-spec(get_type/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Type :: exmpp_stanza:type() | undefined
).

get_type(Stanza=#xmlel{})  ->
    exxml:get_attr(Stanza, <<"type">>, undefined);
get_type(#iq{type = Type}) ->
    Type.

%% @doc Set the type of the stanza.
-spec(set_type/2 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq(),
  Type   :: exmpp_stanza:type())
    -> Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq()
).

set_type(Stanza, Type) when ?Is_Type(Type) ->
    exxml:set_attr(Stanza, <<"type">>, Type);
set_type(IQ, Type) when is_record(IQ, 'iq') andalso ?Is_IQ_Type_Request(Type) ->
    IQ#iq{type = Type, kind = 'request'};
set_type(IQ, Type) when is_record(IQ, 'iq') andalso ?Is_IQ_Type_Response(Type) ->
    IQ#iq{type = Type, kind = 'response'}.

%% @doc Return the language of the stanza.
-spec(get_lang/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Lang :: exmpp_stanza:lang() | undefined
).

get_lang(Stanza = #xmlel{})  ->
    case exxml:get_attr(Stanza, <<"lang">>, undefined) of
        undefined -> exxml:get_attr(Stanza, <<"xml:lang">>);
        Lang      -> Lang
    end;
get_lang(#iq{lang = Lang}) ->
    Lang.

%% @doc Set the lang.
%%
%% If `Lang' is `undefined' or empty, it's removed.
-spec(set_lang/2 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq(),
  Lang   :: exmpp_stanza:lang() | undefined)
    -> Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq()
).

set_lang(Stanza=#xmlel{}, undefined)  ->
    exxml:remove_attr(exxml:remove_attr(Stanza, <<"lang">>), <<"xml:lang">>);
set_lang(Stanza=#xmlel{}, Lang)  ->
    exxml:set_attr(Stanza, <<"lang">>, Lang);
set_lang(IQ, Lang) when is_record(IQ, 'iq') andalso is_binary(Lang)->
    IQ#iq{lang = Lang}.


%% --------------------------------------------------------------------
%% Common operations.
%% --------------------------------------------------------------------

%% @doc Prepare a reply to `Stanza'.
%%
%% @see reply_from_attrs/1.
-spec(reply/1 ::
(
  Stanza::exmpp_stanza:stanza())
    -> Stanza::exmpp_stanza:stanza()
).

reply(Stanza)  ->
    case {get_sender(Stanza), get_recipient(Stanza)} of
        {undefined, undefined} ->
            Stanza;
        {From, undefined} ->
            exxml:remove_attr(exxml:set_attr(Stanza, <<"to">>, From),
                <<"from">>);
        {undefined, To} ->
            exxml:remove_attr(exxml:set_attr(Stanza, <<"from">>, To),
                <<"to">>);
        {From, To} ->
            exxml:set_attr(exxml:set_attr(Stanza, <<"from">>, To),
                <<"to">>, From)
    end.

%% @doc Prepare a reply to `Stanza' with children removed.
-spec(reply_without_content/1 ::
(
  Stanza::exmpp_stanza:stanza())
    ->  Stanza::exmpp_stanza:stanza()
).

reply_without_content(Stanza)  ->
    (reply(Stanza))#xmlel{children = []}.

%% @doc Prepare an error reply to `Stanza'.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec(reply_with_error/2 ::
(
  Stanza :: exmpp_stanza:stanza(),
  _      :: exmpp_stanza:xmlel_error() | exmpp_stanza:error_condition())
    -> Stanza :: exmpp_stanza:stanza()
).

reply_with_error(Stanza, Error_Condition) when is_binary(Error_Condition) ->
    reply_with_error(Stanza, exmpp_stanza:error(Error_Condition));
reply_with_error(Stanza, Xmlel_Error) ->
    stanza_error(reply(Stanza), Xmlel_Error).

%% --------------------------------------------------------------------
%% Stanza-level errors.
%% --------------------------------------------------------------------

-spec(standard_conditions/0
  :: () -> Standard_Conditions::exmpp_stanza:standard_conditions()
).

standard_conditions() ->
    [
     {<<"bad-request">>,             <<"modify">> },
     {<<"conflict">>,                <<"cancel">> },
     {<<"feature-not-implemented">>, <<"cancel">> },
     {<<"forbidden">>,               <<"auth">>   },
     {<<"gone">>,                    <<"modify">> },
     {<<"internal-server-error">>,   <<"wait">>   },
     {<<"item-not-found">>,          <<"cancel">> },
     {<<"jid-malformed">>,           <<"modify">> },
     {<<"not-acceptable">>,          <<"modify">> },
     {<<"not-allowed">>,             <<"cancel">> },
     {<<"not-authorized">>,          <<"auth">>   },
     {<<"payment-required">>,        <<"auth">>   },
     {<<"recipient-unavailable">>,   <<"wait">>   },
     {<<"redirect">>,                <<"modify">> },
     {<<"registration-required">>,   <<"auth">>   },
     {<<"remote-server-not-found">>, <<"cancel">> },
     {<<"remote-server-timeout">>,   <<"wait">>   },
     {<<"resource-constraint">>,     <<"wait">>   },
     {<<"service-unavailable">>,     <<"cancel">> },
     {<<"subscription-required">>,   <<"auth">>   },
     {<<"unexpected-request">>,      <<"wait">>   },
     {<<"undefined-condition">>,     undefined}
    ].

%% @doc Create an `<error/>' element based on the given `Condition'.
-spec(error/1 ::
(
  Error_Condition::exmpp_stanza:error_condition())
    -> Xmlel_Error::exmpp_stanza:xmlel_error()
).

error(Error_Condition) ->
    error(Error_Condition, {undefined, undefined}).

%% @doc Create an `<error/>' element based on the given `Condition'.
-spec(error/2 ::
(
  Error_Condition :: exmpp_stanza:error_condition(),
  {Error_Lang :: exmpp_stanza:error_lang() | undefined,
   Error_Text :: exmpp_stanza:error_text() | undefined})
    -> Xmlel_Error::exmpp_stanza:xmlel_error()
).

error(Error_Condition, {_Error_Lang, undefined = _Error_Text})
  when is_binary(Error_Condition) ->
    set_error_type_from_condition_in_error(
        exxml:element(undefined, <<"error">>, [], [
            exxml:element(?NS_STANZA_ERRORS, Error_Condition, [], [])
        ]),
        Error_Condition);
error(Error_Condition, {undefined = _Error_Lang, Error_Text})
  when    is_binary(Error_Condition)
  andalso is_binary(Error_Text) ->
    set_error_type_from_condition_in_error(
        exxml:element(undefined, <<"error">>, [], [
            exxml:element(?NS_STANZA_ERRORS, Error_Condition, [], []),
            exxml:element(?NS_STANZA_ERRORS, <<"text">>, [],
                [exxml:cdata(Error_Text)])
        ]),
        Error_Condition);
error(Error_Condition, {Error_Lang, Error_Text})
  when    is_binary(Error_Condition)
  andalso is_binary(Error_Text)
  andalso is_binary(Error_Lang) ->
    set_error_type_from_condition_in_error(
        exxml:element(undefined, <<"error">>, [], [
            exxml:element(?NS_STANZA_ERRORS, Error_Condition, [], []),
            exxml:element(?NS_STANZA_ERRORS, <<"text">>,
                [exxml:attr(<<"xml:lang">>, Error_Lang)],
                    [exxml:cdata(Error_Text)])
        ]),
        Error_Condition);
error(Condition, Text) ->
    error(Condition, {undefined, Text}).

%% @doc Transform `Stanza' in a stanza error.
%%
%% The `type' attribute is set and an error condition is added. The
%% caller is still responsible to set or modify the `to' attribute
%% correctly.
%%
%% @see error/2.
%% @see error/3.
-spec(stanza_error/2 ::
(
  Stanza      :: exmpp_stanza:stanza(),
  Xmlel_Error :: exmpp_stanza:xmlel_error())
    -> Stanza :: exmpp_stanza:stanza()
).

stanza_error(Stanza, Xmlel_Error) when ?Is_Xmlel_Error(Xmlel_Error) ->
    set_type(exxml:append_child(Stanza, Xmlel_Error), <<"error">>).

%% @doc Transform `Stanza' in a stanza error.
%%
%% Previous child elements from `Stanza' are not kept.
%%
%% @see stanza_error/2.
-spec(stanza_error_without_original/2 ::
(
  Stanza      :: exmpp_stanza:stanza(),
  Xmlel_Error :: exmpp_stanza:xmlel_error())
    -> Stanza :: exmpp_stanza:stanza()
).

stanza_error_without_original(Stanza, Xmlel_Error)
  when  ?Is_Xmlel_Error(Xmlel_Error) ->
    set_type(exxml:append_child(Stanza, Xmlel_Error), <<"error">>).

%% @doc Tell if the stanza transports an error.
-spec(is_stanza_error/1 ::
(
  Stanza :: exmpp_stanza:stanza())
    -> Is_Stanza_Error::boolean()
).

is_stanza_error(Stanza) ->
    case get_type(Stanza) of
        <<"error">> -> true;
        _           -> false
    end.

%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Return the type of the error element.
-spec(get_error_type/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Error_Type :: exmpp_stanza:error_type() | undefined
).

get_error_type(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, 'no_error_element_found', Stanza});
        Xmlel_Error ->
            get_error_type_from_error(Xmlel_Error)
    end.

%%
-spec(get_error_type_from_error/1 ::
(
  Xmlel_Error::exmpp_stanza:xmlel_error())
    -> Error_Type :: exmpp_stanza:error_type() | undefined
).

get_error_type_from_error(Xmlel_Error) ->
    exxml:get_attr(Xmlel_Error, <<"type">>, undefined).

%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Set the type of the error element.
-spec(set_error_type/2 ::
(
  Stanza     :: exmpp_stanza:stanza(),
  Error_Type :: exmpp_stanza:error_type())
    -> Stanza::exmpp_stanza:stanza()
).

set_error_type(Stanza, Error_Type) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, 'no_error_element_found', Stanza});
        Xmlel_Error ->
            exxml:set_or_replace_child(Stanza,
                set_error_type_in_error(Xmlel_Error, Error_Type))
    end.

%%
-spec(set_error_type_in_error/2 ::
(
  Xmlel_Error :: exmpp_stanza:xmlel_error(),
  Error_Type  :: exmpp_stanza:error_type())
    -> Xmlel_Error::exmpp_stanza:xmlel_error()
).

set_error_type_in_error(Xmlel_Error, Error_Type) when ?Is_Xmlel_Error(Xmlel_Error) ->
    exxml:set_attr(Xmlel_Error, <<"type">>, Error_Type).

%% @throws {stanza_error, error_type, no_error_element_found, Stanza} |
%%         {stanza_error, error_type, invalid_condition, {NS, Condition}}
%% @doc Set the type of the error element, based on the given condition.
-spec(set_error_type_from_condition/2 ::
(
  Stanza          :: exmpp_stanza:stanza(),
  Error_Condition :: exmpp_stanza:error_condition())
    -> Stanza::exmpp_stanza:stanza()
).

set_error_type_from_condition(Stanza, Error_Condition)
  when is_binary(Error_Condition) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, 'no_error_element_found', Stanza});
        Xmlel_Error ->
            exxml:set_or_replace_child(Stanza,
                set_error_type_from_condition_in_error(Xmlel_Error,
                    Error_Condition))
    end.

%%
-spec(set_error_type_from_condition_in_error/2 ::
(
  Xmlel_Error     :: exmpp_stanza:xmlel_error(),
  Error_Condition :: exmpp_stanza:error_condition())
    -> Xmlel_Error :: exmpp_stanza:xmlel_error()
).

set_error_type_from_condition_in_error(Xmlel_Error, Error_Condition)
  when is_binary(Error_Condition) ->
    case lists:keyfind(Error_Condition, 1, standard_conditions()) of
        {Error_Condition, undefined} ->
            Xmlel_Error;
        {Error_Condition, Error_Type} ->
            set_error_type_in_error(Xmlel_Error, Error_Type);
        _ ->
            throw({stanza_error, error_type, 'invalid_condition', Error_Condition})
    end.

%% @throws {stanza_error, condition, no_error_element_found, Stanza} |
%%         {stanza_error, condition, no_condition_found, Error}
%% @doc Return the child element name corresponding to the stanza error
%% condition.
-spec(get_condition/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Error_Condition::exmpp_stanza:error_condition()
).

get_condition(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, condition, 'no_error_element_found', Stanza});
        Xmlel_Error ->
            get_condition_in_error(Xmlel_Error)
    end.

%%
-spec(get_condition_in_error/1 ::
(
  Xmlel_Error::exmpp_stanza:xmlel_error())
    -> Error_Condition::exmpp_stanza:error_condition()
).

get_condition_in_error(Xmlel_Error) when ?Is_Xmlel_Error(Xmlel_Error) ->
    case exxml:get_els(Xmlel_Error) of
        [#xmlel{name = Error_Condition} | _] when Error_Condition /= <<"text">> ->
            Error_Condition;
        _ ->
            % This <error/> element is invalid because the condition must be
            % present (and first).
            throw({stanza_error, condition, 'no_condition_found', Xmlel_Error})
    end.

%% @throws {stanza_error, text, no_error_element_found, Stanza}
%% @doc Return the text that describes the error.
%%
%% If there is no `<text/>' element, an empty string is returned.
-spec(get_text/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
    -> Error_Text :: exmpp_stanza:error_text() | undefined
).

get_text(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, text, 'no_error_element_found', Stanza});
        Xmlel_Error ->
            get_text_in_error(Xmlel_Error)
    end.

%%
-spec(get_text_in_error/1 ::
(
  Xmlel_Error::exmpp_stanza:xmlel_error())
    -> Error_Text :: exmpp_stanza:error_text() | undefined
).

get_text_in_error(Xmlel_Error) when ?Is_Xmlel_Error(Xmlel_Error) ->
    exxml:get_path(Xmlel_Error, [{'element', <<"text">>}, 'cdata']).

%% --------------------------------------------------------------------
%% Serialization wrappers.
%% --------------------------------------------------------------------

%% @doc Serialize a stanza using common XMPP default namespaces.
-spec(to_iolist/1 ::
(
  Stanza :: exmpp_stanza:stanza() | exmpp_iq:iq())
  -> IO_List::iolist()
).

to_iolist(IQ) when is_record(IQ, 'iq') ->
    to_iolist(exmpp_iq:iq_to_xmlel(IQ));
to_iolist(Stanza)  ->
    exxml:doc_to_iolist(Stanza).

