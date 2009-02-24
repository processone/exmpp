% $Id$

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

% IQ stanza.
-record(iq, {
  kind    :: request | response,
  type    :: get | set | result | error,
  id      :: binary() | undefined,
  ns      :: xmlname() | undefined,
  payload :: #xmlel{} | undefined,
  error   :: #xmlel{} | undefined,
  lang    :: binary() | undefined,
  iq_ns   :: xmlname() | undefined
}).

% --------------------------------------------------------------------
% Macros for common tests.
% --------------------------------------------------------------------

% Guard expression to test a stanza as defined by XMPP-IM.
-define(IS_IM_STANZA(El), (
  El#xmlel.ns == ?NS_JABBER_CLIENT orelse
  El#xmlel.ns == ?NS_JABBER_SERVER orelse
  El#xmlel.ns == ?NS_COMPONENT_ACCEPT orelse
  El#xmlel.ns == ?NS_COMPONENT_CONNECT
)).

% Guard expression to test a message.
-define(IS_MESSAGE(El), (
  ?IS_IM_STANZA(El) andalso El#xmlel.name == 'message'
)).

% Guard expression to test a presence.
-define(IS_PRESENCE(El), (
  ?IS_IM_STANZA(El) andalso El#xmlel.name == 'presence'
)).

% Guard expression to test an IQ.
-define(IS_IQ(El), (
  ?IS_IM_STANZA(El) andalso El#xmlel.name == 'iq'
)).
-define(IS_IQ_RECORD(IQ), (
  is_record(IQ, iq)
)).

% Guard expression to test a JID.
-define(IS_JID(Jid), (
  element(1, Jid) =:= 'jid' andalso tuple_size(Jid) =:= 5
)).
