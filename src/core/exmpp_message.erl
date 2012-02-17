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
%% The module <strong>{@module}</strong> provides helpers to do message
%% common operations.

-module(exmpp_message).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Message creation.
-export([
    normal/0,
    normal/1,
    normal/2,
    chat/0,
    chat/1,
    chat/2,
    groupchat/0,
    groupchat/1,
    groupchat/2,
    headline/0,
    headline/1,
    headline/2,
    error/2
]).

%% Message standard attributes.
-export([
    is_message/1,
    get_type/1,
    set_type/2,
    get_subject/1,
    set_subject/2,
    get_body/1,
    set_body/2,
    get_thread/1,
    set_thread/2
]).

%%

-define(EMPTY_MESSAGE, exxml:element(<<"message">>)).

-define(Is_Xmlel_Error(Xmlel_Error),
(
  Xmlel_Error#xmlel.name == <<"error">>
)).

-define(Is_Message_Type(Type),
(
  Type == <<"normal">>    orelse
  Type == <<"chat">>      orelse
  Type == <<"headline">>  orelse
  Type == <<"groupchat">> orelse
  Type == <<"error">>
)).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-export_type([
  subject/0,
  body/0,
  thread/0
]).

-type(body()    :: binary()).
-type(subject() :: binary()).
-type(thread()  :: binary()).

%% --------------------------------------------------------------------
%% Message creation.
%% --------------------------------------------------------------------

%% @doc Create an empty message stanza.
-spec(normal/0 :: () -> Message_Normal::exmpp_stanza:message_normal()).

normal() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"normal">>).

%% @doc Create a message stanza with a given body.
-spec(normal/1 ::
(
  Body::exmpp_message:body())
    -> Message_Normal::exmpp_stanza:message_normal()
).

normal(Body) ->
    set_body(normal(), Body).

%% @doc Create a message stanza with given subject and body.
-spec(normal/2 ::
(
  Subject :: exmpp_message:subject(),
  Body    :: exmpp_message:body())
    -> Message_Normal::exmpp_stanza:message_normal()
).

normal(Subject, Body) ->
    set_subject(normal(Body), Subject).


%% @doc Create an empty chat message stanza.
-spec(chat/0 :: () -> Message_Chat::exmpp_stanza:message_chat()).

chat() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"chat">>).

%% @doc Create a chat message stanza with a given body.
-spec(chat/1 ::
(
  Body :: exmpp_message:body())
    -> Message_Chat::exmpp_stanza:message_chat()
).

chat(Body) ->
    set_body(chat(), Body).

%% @doc Create a chat message stanza with given subject and body.
-spec(chat/2 ::
(
  Subject :: exmpp_message:subject(),
  Body    :: exmpp_message:body())
    -> Message_Chat::exmpp_stanza:message_chat()
).

chat(Subject, Body) ->
    set_subject(chat(Body), Subject).

%% @doc Create an empty groupchat message stanza.
-spec(groupchat/0 :: () -> Message_Groupchat::exmpp_stanza:message_groupchat()).

groupchat() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"groupchat">>).

%% @doc Create a groupchat message stanza with a given body.
-spec(groupchat/1 ::
(
  Body :: exmpp_message:body())
    -> Message_Groupchat::exmpp_stanza:message_groupchat()
).

groupchat(Body) ->
    set_body(groupchat(), Body).

%% @doc Create a groupchat message stanza with given subject and body.
-spec(groupchat/2 ::
(
  Subject :: exmpp_message:subject(),
  Body    :: exmpp_message:body())
    -> Message_Groupchat::exmpp_stanza:message_groupchat()
).

groupchat(Subject, Body) ->
    set_subject(groupchat(Body), Subject).

%% @doc Create an empty headline message stanza.
-spec(headline/0 :: () -> Message_Headline::exmpp_stanza:message_headline()).

headline() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"headline">>).

%% @doc Create a headline message stanza with a given body.
-spec(headline/1 ::
(
  Body :: exmpp_message:body())
    -> Message_Headline::exmpp_stanza:message_headline()
).

headline(Body) ->
    set_body(headline(), Body).

%% @doc Create a headline message stanza with given subject and body.
-spec(headline/2 ::
(
  Subject :: exmpp_message:subject(),
  Body    :: exmpp_message:body())
    -> Message_Headline::exmpp_stanza:message_headline()
).

headline(Subject, Body) ->
    set_subject(headline(Body), Subject).

%% @doc Prepare a message stanza to notify an error.
%%
%% If `Error' is a binary, it must be a standard condition defined by
%% XMPP Core.
-spec(error/2 ::
(
  Message :: exmpp_stanza:message(),
  _       :: exmpp_stanza:error_condition() | exmpp_stanza:xmlel_error())
    -> Message::exmpp_stanza:message()
).

error(Message, Error_Condition) when is_binary(Error_Condition) ->
    error(Message, exmpp_stanza:error(Error_Condition));
error(Message, Xmlel_Error)
  when ?IS_MESSAGE(Message) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    exmpp_stanza:reply_with_error(Message, Xmlel_Error).

%% --------------------------------------------------------------------
%% Message standard attributes.
%% --------------------------------------------------------------------

%% @doc Tell if `Xmlel' is a message.
%%
%% You should probably use the `IS_MESSAGE(Xmlel)' guard expression.
-spec(is_message/1 ::
(
  Message :: exmpp_stanza:message() | any())
    -> Is_Message::boolean()
).

is_message(Message) when ?IS_MESSAGE(Message) -> true;
is_message(_)                                 -> false.

%% @doc Return the type of the given `<message/>'.
-spec(get_type/1 ::
(
  Message::exmpp_stanza:message())
    -> Type::exmpp_stanza:message_type()
).

get_type(Message) when ?IS_MESSAGE(Message) ->
    exxml:get_attr(Message, <<"type">>, <<"normal">>).

%% @doc Set the type of the given `<message/>'.
-spec(set_type/2 ::
(
  Message :: exmpp_stanza:message(),
  Type    :: exmpp_stanza:message_type())
    -> Message::exmpp_stanza:message()
).

set_type(Message, Type) when ?Is_Message_Type(Type) ->
    exmpp_stanza:set_type(Message, Type).

%% @doc Return the subject of the message.
-spec(get_subject/1 ::
(
  Message::exmpp_stanza:message())
    -> Subject :: exmpp_message:subject() | undefined
).

get_subject(Message) when ?IS_MESSAGE(Message) ->
    case exxml:get_el(Message, <<"subject">>) of
        undefined     -> undefined;
        Xmlel_Subject -> exxml:get_cdata(Xmlel_Subject)
    end.

%% @doc Set the `<subject/>' field of a message stanza.
-spec(set_subject/2 ::
(
  Message :: exmpp_stanza:message(),
  Subject :: exmpp_message:subject())
    -> Message :: exmpp_stanza:message()
).

set_subject(Message, Subject) when ?IS_MESSAGE(Message) ->
    exxml:set_or_replace_child(Message,
        exxml:element(undefined, <<"subject">>, [], [exxml:cdata(Subject)])
    ).

%% @doc Return the body of the message.
-spec(get_body/1 ::
(
  Message::exmpp_stanza:message())
    -> Body :: exmpp_message:body() | undefined
).

get_body(Message) when ?IS_MESSAGE(Message) ->
    case exxml:get_el(Message, <<"body">>) of
        undefined  -> undefined;
        Xmlel_Body -> exxml:get_cdata(Xmlel_Body)
    end.

%% @doc Set the `<body/>' field of a message stanza.
-spec(set_body/2 ::
(
  Message :: exmpp_stanza:message(),
  Body    :: exmpp_message:body())
    -> Message :: exmpp_stanza:message()
).

set_body(Message, Body) when ?IS_MESSAGE(Message) ->
    exxml:set_or_replace_child(Message,
        exxml:element(undefined, <<"body">>, [], [exxml:cdata(Body)])
    ).

%% @doc Return the thread of the message.
-spec(get_thread/1 ::
(
  Message::exmpp_stanza:message())
    -> Thread :: exmpp_message:thread() | undefined
).

get_thread(Message) when ?IS_MESSAGE(Message) ->
    case exxml:get_el(Message, <<"thread">>) of
        undefined    -> undefined;
        Xmlel_Thread -> exxml:get_cdata(Xmlel_Thread)
    end.

%% @doc Set the `<thread/>' field of a message stanza.
-spec(set_thread/2 ::
(
  Message :: exmpp_stanza:message(),
  Thread  :: exmpp_message:thread())
    -> Message :: exmpp_stanza:message()
).

set_thread(Message, Thread) when ?IS_MESSAGE(Message) ->
    exxml:set_or_replace_child(Message,
        exxml:element(undefined, <<"thread">>, [], [exxml:cdata(Thread)])
    ).

