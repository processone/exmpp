% $Id$

% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Elements without namespace support.
-record(xmlelement, {
  name,                   % Tag name
  attrs = [],             % Attribute list
  children = undefined    % Children (tags or CDATA)
}).

-record(xmlendelement, {
  name
}).

% Elements WITH namespace support.
-record(xmlnselement, {
  ns = undefined,         % Tag namespace
  prefix = undefined,     % Namespace prefix
  default_ns = undefined, % Current default namespace
  name,                   % Tag name
  attrs = [],             % Attribute list
  children = undefined    % Children (tags or CDATA)
}).

-record(xmlnsendelement, {
  ns = undefined,
  prefix = undefined,
  name
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
  element                 % #xmlnselement
}).

% Depth 1 element, inside a stream.
-record(xmlstreamelement, {
  element                 % #xmlnselement
}).

% Stream end.
-record(xmlstreamend, {
  endelement              % xmlnsendelement
}).

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

-record(iq, {
  id = "",
  type,
  xmlns = "",
  lang = "",
  sub_el
}).

% --------------------------------------------------------------------
% Namespaces used in exmpp_factory.
% --------------------------------------------------------------------

-define(NS_XML,             'http://www.w3.org/XML/1998/namespace').
-define(NS_XMPP,            'http://etherx.jabber.org/streams').
-define(NS_JABBER_CLIENT,   'jabber:client').
-define(NS_JABBER_SERVER,   'jabber:server').
-define(NS_JABBER_AUTH,     'jabber:iq:auth').
-define(NS_JABBER_PRIVACY,  'jabber:iq:privacy').
-define(NS_JABBER_REGISTER, 'jabber:iq:register').
-define(NS_JABBER_ROSTER,   'jabber:iq:roster').
-define(NS_BIND,            'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_JABBER_SESSION,  'urn:ietf:params:xml:ns:xmpp-session').
-define(NS_XMPP_STREAMS,    'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_XMPP_STANZAS,    'urn:ietf:params:xml:ns:xmpp-stanzas').
-define(NS_TLS,             'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,            'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_COMPRESS,        'http://jabber.org/features/compress').
-define(NS_PUBSUB,          'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,   'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,    'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,    'http://jabber.org/protocol/pubsub#owner').

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

% --------------------------------------------------------------------
% Defines for jlib.
% --------------------------------------------------------------------

-define(NS_DELAY, "jabber:x:delay").

% vim:ft=erlang:
