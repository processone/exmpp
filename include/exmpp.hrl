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

% --------------------------------------------------------------------
% Namespace and prefix macros.
% --------------------------------------------------------------------

% Defined by XML.
-define(NS_XML,                 'http://www.w3.org/XML/1998/namespace').
-define(NS_XML_pfx,             "xml").

% Defined by XMPP Core (RFC 3920).
-define(NS_XMPP,                'http://etherx.jabber.org/streams').
-define(NS_XMPP_pfx,            "stream").
-define(NS_STREAM_ERRORS,       'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_TLS,                 'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,                'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_BIND,                'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_DIALBACK,            'jabber:server:dialback').
-define(NS_DIALBACK_pfx,        "db").
-define(NS_STANZA_ERRORS,       'urn:ietf:params:xml:ns:xmpp-stanzas').

% Defined by XMPP-IM (RFC 3921).
-define(NS_JABBER_CLIENT,       'jabber:client').
-define(NS_JABBER_SERVER,       'jabber:server').
-define(NS_SESSION,             'urn:ietf:params:xml:ns:xmpp-session').
-define(NS_PRIVACY,             'jabber:iq:privacy').
-define(NS_ROSTER,              'jabber:iq:roster').

% Defined by XEP-0054: vcard-temp.
-define(NS_VCARD,               'vcard-temp').

% Defined by XEP-0060: Publish-Subscribe.
-define(NS_PUBSUB,              'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,       'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,        'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,        'http://jabber.org/protocol/pubsub#owner').

% Defined by XEP-0077: In-Band Registration.
-define(NS_INBAND_REGISTER,     'jabber:iq:register').

% Defined by XEP-0078: Non-SASL Authentication.
-define(NS_LEGACY_AUTH,         'jabber:iq:auth').

% Defined by XEP-0114: Jabber Component Protocol.
-define(NS_COMPONENT_ACCEPT,    'jabber:component:accept').
-define(NS_COMPONENT_CONNECT,   'jabber:component:connect').

% Defined by XEP-0138: Stream Compression.
-define(NS_COMPRESS,            'http://jabber.org/features/compress').

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
