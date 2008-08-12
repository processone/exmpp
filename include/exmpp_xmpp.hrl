% $Id$

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

% IQ stanza.
-record(iq, {
  kind,    % 'request' or 'response'.
  type,    % 'get', 'set', 'result' or 'error'.
  id,      % ID of the IQ.
  ns,      % Namespace of the payload.
  payload, % The request or the response transported by this IQ.
  error,   % Error element (if type == 'error').
  lang,    % Language of this IQ.
  iq_ns    % Namespace of the IQ (eg. 'jabber:client').
}).

% JID.
-record(jid, {
  node,
  domain,
  resource,
  lnode,
  ldomain,
  lresource
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
-define(IS_JID(Jid),
  record(Jid, jid)
).
