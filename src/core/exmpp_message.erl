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

-define(EMPTY_MESSAGE, {xmlel, <<"message">>, [], []}).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-type(messagetype() :: binary()). % <<"normal">> | <<"chat">> | <<"groupchat">>| <<"headline">> | <<"error">>

%% --------------------------------------------------------------------
%% Message creation.
%% --------------------------------------------------------------------

%% @spec () -> Message
%% @doc Create an empty message stanza.
%%
%% The default namespace is `jabber:client'.

-spec normal () -> exml:xmlel().

normal() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"normal">>).

%% @spec (Body) -> Message
%%     Body = binary()
%%     Message = exml:xmlel()
%% @doc Create a message stanza with a given body.
%%
-spec normal (binary() ) -> xmlel().

normal(Body) ->
	set_body(normal(), Body).

%% @spec (Subject, Body) -> Message
%%     Subject = binary()
%%     Body = binary()
%%     Message = exml:xmlel()
%% @doc Create a message stanza with given subject and body.
%%

-spec normal (binary() , binary() ) -> xmlel().

normal(Subject, Body) ->
	set_subject(normal(Body), Subject).


%% @spec () -> Message
%%     Message = exml:xmlel()
%% @doc Create an empty chat message stanza.
%%
%% The default namespace is `jabber:client'.

-spec chat () -> xmlel().

chat() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"chat">>).

%% @spec (Body) -> Message
%%     Body = binary()
%%     Message = exml:xmlel()
%% @doc Create a chat message stanza with a given body.
%%

-spec chat (binary() ) -> exml:xmlel().

chat(Body) ->
	set_body(chat(), Body).

%% @spec (Subject, Body) -> Message
%%     Subject = binary()
%%     Body =  binary()
%%     Message = exml:xmlel()
%% @doc Create a chat message stanza with given subject and body.
%%

-spec chat (binary() , binary() ) -> exml:xmlel().

chat(Subject, Body) ->
	set_subject(chat(Body), Subject).


%% @spec () -> Message
%% @doc Create an empty groupchat message stanza.
%%

-spec groupchat () -> exml:xmlel().

groupchat() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"groupchat">>).

%% @spec (Body) -> Message
%%     Body = binary()
%%     Message = exml:xmlel()
%% @doc Create a groupchat message stanza with a given body.
%%

-spec groupchat (binary() ) -> exml:xmlel().

groupchat(Body) ->
	set_body(groupchat(), Body).

%% @spec (Subject, Body) -> Message
%%     Subject =  binary()
%%     Body =  binary()
%%     Message = exml:xmlel()
%% @doc Create a groupchat message stanza with given subject and body.
%%

-spec groupchat (binary() , binary() ) -> xmlel().

groupchat(Subject, Body) ->
    set_subject(groupchat(Body), Subject).

%% @spec () -> Message
%% @doc Create an empty headline message stanza.
%%

-spec headline () -> exml:xmlel().

headline() ->
    exmpp_stanza:set_type(?EMPTY_MESSAGE, <<"headline">>).

%% @spec (Body) -> Message
%%     Body =  binary()
%%     Message = exml:xmlel()
%% @doc Create a headline message stanza with a given body.
%%

-spec headline (binary() ) -> exml:xmlel().

headline(Body) ->
	set_body(headline(), Body).

%% @spec (Subject, Body) -> Message
%%     Subject = binary()
%%     Body = binary()
%%     Message = exml:xmlel()
%% @doc Create a headline message stanza with given subject and body.
%%

-spec headline (binary() , binary() ) -> exml:xmlel().

headline(Subject, Body) ->
    set_subject(headline(Body), Subject).


%% @spec (Message, Error) -> New_Message
%%     Message = exml:xmlel()
%%     Error = exml:xmlel() | binary()
%%     New_Message = exml:xmlel()
%% @doc Prepare a message stanza to notify an error.
%%
%% If `Error' is a binary, it must be a standard condition defined by
%% XMPP Core.

-spec error
(exml:xmlel(), exml:xmlel() | binary()) -> exml:xmlel().

error(Message, Condition) when is_binary(Condition) ->
    Error = exmpp_stanza:error(Condition),
    error(Message, Error);
error(Message, Error) when ?IS_MESSAGE(Message) ->
    exmpp_stanza:reply_with_error(Message, Error).

%% --------------------------------------------------------------------
%% Message standard attributes.
%% --------------------------------------------------------------------

%% @spec (El) -> boolean
%%     El = exml:xmlel()
%% @doc Tell if `El' is a message.
%%
%% You should probably use the `IS_MESSAGE(El)' guard expression.

-spec is_message
(exml:xmlel()) -> boolean().

is_message(Message) when ?IS_MESSAGE(Message) -> true;
is_message(_El)                               -> false.

%% @spec (Message) -> Type
%%     Message = exml:xmlel()
%%     Type = <<"chat">> | <<"groupchat">> | <<"headline">> | <<"normal">> | <<"error">>
%% @doc Return the type of the given `<message/>'.

-spec get_type (exml:xmlel()) -> messagetype().

get_type(Message) when ?IS_MESSAGE(Message) ->
	exml:get_attribute(Message, <<"type">>, <<"normal">>).

%% @spec (Message, Type) -> New_Message
%%     Message = exml:xmlel()
%%     Type = binary()
%%     New_Message = exml:xmlel()
%% @doc Set the type of the given `<message/>'.
%%

-spec set_type (xmlel(), messagetype() ) -> exml:xmlel().

set_type(Message, Type) when is_binary(Type) ->
    exmpp_stanza:set_type(Message, Type).

%% @spec (Message) -> Subject | undefined
%%     Message = exml:xmlel()
%%     Subject = binary()
%% @doc Return the subject of the message.

-spec get_subject (exml:xmlel()) -> binary() | undefined.

get_subject(Message) when ?IS_MESSAGE(Message) ->
    case exml:get_element(Message, <<"subject">>) of
        undefined ->
            undefined;
        Subject_El ->
            exml:get_cdata(Subject_El)
    end.

%% @spec (Message, Subject) -> New_Message
%%     Message = exml:xmlel()
%%     Subject =  binary()
%%     New_Message = exml:xmlel()
%% @doc Set the `<subject/>' field of a message stanza.
%%

-spec set_subject (exml:xmlel(), binary() ) -> exml:xmlel().

set_subject(Message, Subject) when ?IS_MESSAGE(Message) ->
	New_Subject_El = {xmlel, <<"subject">>, [], [{cdata, Subject}]},
	exml:set_or_replace_child(Message, New_Subject_El).

%% @spec (Message) -> Body | undefined
%%     Message = exml:xmlel()
%%     Body = binary()
%% @doc Return the body of the message.

-spec get_body (exml:xmlel()) -> binary() | undefined.

get_body(Message) when ?IS_MESSAGE(Message) ->
    case exml:get_element(Message, <<"body">>) of
        undefined ->
            undefined;
        Body_El ->
            exml:get_cdata(Body_El)
    end.

%% @spec (Message, Body) -> New_Message
%%     Message = exml:xmlel()
%%     Body = binary()
%%     New_Message = exml:xmlel()
%% @doc Set the `<body/>' field of a message stanza.
%%

-spec set_body (exml:xmlel(), binary() ) -> exml:xmlel().

set_body(Message, Body) when ?IS_MESSAGE(Message) ->
	New_Body_El = {xmlel, <<"body">>, [], [{cdata, Body}]},
	exml:set_or_replace_child(Message, New_Body_El).

%% @spec (Message) -> Thread | undefined
%%     Message = exml:xmlel()
%%     Thread = binary()
%% @doc Return the thread of the message.

-spec get_thread (exml:xmlel()) -> binary() | undefined.

get_thread(Message) when ?IS_MESSAGE(Message) ->
    case exml:get_element(Message, <<"thread">>) of
        undefined ->
            undefined;
        Thread_El ->
            exml:get_cdata(Thread_El)
    end.

%% @spec (Message, Thread) -> New_Message
%%     Message = exml:xmlel()
%%     Thread = binary()
%%     New_Message = exml:xmlel()
%% @doc Set the `<thread/>' field of a message stanza.
%%

-spec set_thread (exml:xmlel(), binary()) -> exml:xmlel().

set_thread(Message, Thread) when ?IS_MESSAGE(Message) ->
	New_Thread_El = {xmlel, <<"thread">>, [], [{cdata, Thread}]},
	exml:set_or_replace_child(Message, New_Thread_El).
