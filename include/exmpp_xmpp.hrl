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
-define(IS_IM_STANZA(Stanza),
  Stanza#xmlel.ns == ?NS_JABBER_CLIENT;
  Stanza#xmlel.ns == ?NS_JABBER_SERVER
).

% Guard expression to test a message.
-define(IS_MESSAGE(Message),
  Message#xmlel.name == 'message', Message#xmlel.ns == ?NS_JABBER_CLIENT;
  Message#xmlel.name == 'message', Message#xmlel.ns == ?NS_JABBER_SERVER
).

% Guard expression to test a presence.
-define(IS_PRESENCE(Presence),
  Presence#xmlel.name == 'presence', Presence#xmlel.ns == ?NS_JABBER_CLIENT;
  Presence#xmlel.name == 'presence', Presence#xmlel.ns == ?NS_JABBER_SERVER
).

% Guard expression to test an IQ.
-define(IS_IQ(IQ),
  IQ#xmlel.name == 'iq', IQ#xmlel.ns == ?NS_JABBER_CLIENT;
  IQ#xmlel.name == 'iq', IQ#xmlel.ns == ?NS_JABBER_SERVER
).

% Guard expression to test a JID.
-define(IS_JID(Jid),
  record(Jid, jid)
).
