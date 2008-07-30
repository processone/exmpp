% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helper to do message
%% common operations.

-module(exmpp_message).
-vsn('$Revision$').

-include("exmpp.hrl").

% Message standard attributes.
-export([
  is_message/1,
  get_type/1,
  get_subject/1,
  get_body/1,
  get_thread/1
]).

% --------------------------------------------------------------------
% Message standard attributes.
% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a message.
%%
%% You should probably use the `IS_MESSAGE(Message)' guard expression.

is_message(Message) when ?IS_MESSAGE(Message) -> true;
is_message(_El)                               -> false.

%% @spec (Message) -> Type
%%     Message = exmpp_xml:xmlel()
%%     Type = chat | groupchat | headline | normal | error | undefined
%% @doc Return the type of the given `<message/>'.

get_type(Message) ->
    case exmpp_stanza:get_type(Message) of
        "chat"      -> 'chat';
        "groupchat" -> 'groupchat';
        "headline"  -> 'headline';
        "normal"    -> 'normal';
        "error"     -> 'error';
        _           -> undefined
    end.

%% @spec (Message) -> Subject | undefined
%%     Message = exmpp_xml:xmlel()
%%     Subject = binary()
%% @doc Return the subject of the message.

get_subject(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'subject') of
        undefined ->
            undefined;
        Subject_El ->
            exmpp_xml:get_cdata(Subject_El)
    end.

%% @spec (Message) -> Body | undefined
%%     Message = exmpp_xml:xmlel()
%%     Body = binary()
%% @doc Return the body of the message.

get_body(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'body') of
        undefined ->
            undefined;
        Body_El ->
            exmpp_xml:get_cdata(Body_El)
    end.

%% @spec (Message) -> Thread | undefined
%%     Message = exmpp_xml:xmlel()
%%     Thread = binary()
%% @doc Return the thread of the message.

get_thread(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'thread') of
        undefined ->
            undefined;
        Thread_El ->
            exmpp_xml:get_cdata(Thread_El)
    end.
