% $Id$

% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Elements without namespace support.
-record(xmlelement, {
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
}).

% Elements WITH namespace support.
-record(xmlel, {
  ns = undefined,         % Element namespace
  declared_ns = [],       % Declared namespaces in this element
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
}).

% Attributes WITH namespace support.
-record(xmlattr, {
  ns = undefined,
  prefix = undefined,
  name,
  value
}).

% Character data.
-record(xmlcdata, {
  cdata = []              % Character data
}).

% XML end tag.
% To use when 'children' is undefined in xmlel or xmlelement.
-record(xmlendtag, {
  ns = undefined,
  prefix = undefined,
  name
}).

% Processing Instruction.
-record(xmlpi, {
  target,
  value
}).

% --------------------------------------------------------------------
% Records to represent events.
% --------------------------------------------------------------------

% Stream start.
-record(xmlstreamstart, {
  element                 % #xmlel
}).

% Depth 1 element, inside a stream.
-record(xmlstreamelement, {
  element                 % #xmlel
}).

% Stream end.
-record(xmlstreamend, {
  endtag                  % xmlnsendelement
}).

% --------------------------------------------------------------------
% Macros for common tests.
% --------------------------------------------------------------------

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

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

-record(jid, {
  node,
  domain,
  resource,
  lnode,
  ldomain,
  lresource
}).

% --------------------------------------------------------------------
% Namespaces used in exmpp_factory.
% --------------------------------------------------------------------

-define(NS_XML,             'http://www.w3.org/XML/1998/namespace').
-define(NS_XMPP,            'http://etherx.jabber.org/streams').
-define(NS_JABBER_CLIENT,   'jabber:client').
-define(NS_JABBER_SERVER,   'jabber:server').
-define(NS_JABBER_DIALBACK, 'jabber:server:dialback').
-define(NS_JABBER_AUTH,     'jabber:iq:auth').
-define(NS_JABBER_PRIVACY,  'jabber:iq:privacy').
-define(NS_JABBER_REGISTER, 'jabber:iq:register').
-define(NS_JABBER_ROSTER,   'jabber:iq:roster').
-define(NS_BIND,            'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_SESSION,         'urn:ietf:params:xml:ns:xmpp-session').
-define(NS_XMPP_STREAMS,    'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_XMPP_STANZAS,    'urn:ietf:params:xml:ns:xmpp-stanzas').
-define(NS_TLS,             'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,            'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_COMPRESS,        'http://jabber.org/features/compress').
-define(NS_PUBSUB,          'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,   'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,    'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,    'http://jabber.org/protocol/pubsub#owner').
-define(NS_VCARD,           'vcard-temp').

% --------------------------------------------------------------------
% Presence types and status
% --------------------------------------------------------------------

-define(P_AVAILABLE, available).
-define(P_UNAVAILABLE, unavailable).
-define(P_INVISIBLE, invisible).
-define(P_AWAY, away).
-define(P_CHAT, chat).
-define(P_DND, dnd).
-define(P_XA, xa).

% --------------------------------------------------------------------
% Special case value
% --------------------------------------------------------------------

-define(NO_ELEMENT, no_element).

% vim:ft=erlang:
