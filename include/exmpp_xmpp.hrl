% $Id$

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

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
  El#xmlel.ns == ?NS_JABBER_SERVER
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

% Guard expression to test a JID.
-define(IS_JID(Jid),
  record(Jid, jid)
).
