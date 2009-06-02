%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating entity
%% side of privacy lists management.

-module(exmpp_client_privacy).
-vsn('$Revision$').

-include("exmpp.hrl").

%% Creating stanza.
-export([
	 ack_list_push/1
	]).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (Push_IQ) -> Ack_IQ
%%     Push_IQ = exmpp_xml:xmlel()
%%     Ack_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq/>' result to acknowledge the push.

ack_list_push(Push_IQ) ->
    exmpp_iq:result(Push_IQ).
