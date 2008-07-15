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
-define(NS_XML,                      'http://www.w3.org/XML/1998/namespace').
-define(NS_XML_pfx,                  "xml").

% Defined by XMPP Core (RFC 3920).
-define(NS_XMPP,                     'http://etherx.jabber.org/streams').
-define(NS_XMPP_pfx,                 "stream").
-define(NS_STREAM_ERRORS,            'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_TLS,                      'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,                     'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_BIND,                     'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_DIALBACK,                 'jabber:server:dialback').
-define(NS_DIALBACK_pfx,             "db").
-define(NS_STANZA_ERRORS,            'urn:ietf:params:xml:ns:xmpp-stanzas').

% Defined by XMPP-IM (RFC 3921).
-define(NS_JABBER_CLIENT,            'jabber:client').
-define(NS_JABBER_SERVER,            'jabber:server').
-define(NS_SESSION,                  'urn:ietf:params:xml:ns:xmpp-session').
-define(NS_ROSTER,                   'jabber:iq:roster').

% Defined by End-to-End Signing and Object Encryption for XMPP (RFC 3923).
-define(NS_E2E,                      'urn:ietf:params:xml:ns:xmpp-e2e').

% Defined by XEP-0004: Data Forms.
-define(NS_DATA_FORMS,               'jabber:x:data').

% Defined by XEP-0009: Jabber-RPC.
-define(NS_RPC,                      'jabber:iq:rpc').

% Defined by XEP-0012: Last Activity.
-define(NS_LAST_ACTIVITY,            'jabber:iq:last').

% Defined by XEP-0013: Flexible Offline Message Retrieval.
-define(NS_OFFLINE,                  'http://jabber.org/protocol/offline').

% Defined by XEP-0016: Privacy Lists.
-define(NS_PRIVACY,                  'jabber:iq:privacy').

% Defined by XEP-0020: Feature Negotiation.
-define(NS_FEATURE_NEG,              'http://jabber.org/protocol/feature-neg').

% Defined by XEP-0027: Current Jabber OpenPGP Usage.
-define(NS_PGP_ENCRYPTED,            'jabber:x:encrypted').
-define(NS_PGP_SIGNED,               'jabber:x:signed').

% Defined by XEP-0030: Service Discovery.
-define(NS_DISCO_INFO,               'http://jabber.org/protocol/disco#info').
-define(NS_DISCO_ITEMS,              'http://jabber.org/protocol/disco#items').

% Defined by XEP-0033: Extended Stanza Addressing.
-define(NS_ADDRESS,                  'http://jabber.org/protocol/address').

% Defined by XEP-0045: Multi-User Chat.
-define(NS_MUC,                      'http://jabber.org/protocol/muc').
-define(NS_MUC_ADMIN,                'http://jabber.org/protocol/muc#admin').
-define(NS_MUC_OWNER,                'http://jabber.org/protocol/muc#owner').
-define(NS_MUC_UNIQUE,               'http://jabber.org/protocol/muc#unique').
-define(NS_MUC_USER,                 'http://jabber.org/protocol/muc#user').

% Defined by XEP-0047: In-Band Bytestreams.
-define(NS_IBB,                      'http://jabber.org/protocol/ibb').

% Defined by XEP-0048: Bookmarks.
-define(NS_BOOKMARKS,                'storage:bookmarks').

% Defined by XEP-0049: Private XML Storage.
-define(NS_PRIVATE,                  'jabber:iq:private').

% Defined by XEP-0050: Ad-Hoc Commands.
-define(NS_ADHOC,                    'http://jabber.org/protocol/commands').

% Defined by XEP-0054: vcard-temp.
-define(NS_VCARD,                    'vcard-temp').

% Defined by XEP-0055: Jabber Search.
-define(NS_SEARCH,                   'jabber:iq:search').

% Defined by XEP-0059: Result Set Management.
-define(NS_RSM,                      'http://jabber.org/protocol/rsm').

% Defined by XEP-0060: Publish-Subscribe.
-define(NS_PUBSUB,
  'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,
  'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,
  'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,
  'http://jabber.org/protocol/pubsub#owner').

% Defined by XEP-0065: SOCKS5 Bytestreams.
-define(NS_BYTESTREAMS,              'http://jabber.org/protocol/bytestreams'.

% Defined by XEP-0066: Out of Band Data.
-define(NS_OOBD_IQ,                  'jabber:iq:oob').
-define(NS_OOBD_X,                   'jabber:x:oob').

% Defined by XEP-0070: Verifying HTTP Requests via XMPP.
-define(NS_HTTP_AUTH,                'http://jabber.org/protocol/http-auth').

% Defined by XEP-0071: XHTML-IM.
-define(NS_XHTML_IM,                 'http://jabber.org/protocol/xhtml-im').
-define(NS_XHTML,                    'http://www.w3.org/1999/xhtml').

% Defined by XEP-0072: SOAP Over XMPP.
-define(NS_SOAP_FAULT,               'http://jabber.org/protocol/soap#fault').

% Defined by XEP-0077: In-Band Registration.
-define(NS_INBAND_REGISTER,          'jabber:iq:register').
-define(NS_INBAND_REGISTER_FEAT,     'http://jabber.org/features/iq-register').

% Defined by XEP-0078: Non-SASL Authentication.
-define(NS_LEGACY_AUTH,              'jabber:iq:auth').
-define(NS_LEGACY_AUTH_FEAT,         'http://jabber.org/features/iq-aut').

% Defined by XEP-0079: Advanced Message Processing.
-define(NS_AMP,                      'http://jabber.org/protocol/amp').
-define(NS_AMP_ERRORS,               'http://jabber.org/protocol/amp#error').
-define(NS_AMP_FEAT,                 'http://jabber.org/features/amp').

% Defined by XEP-0080: User Location.
-define(NS_GEOLOC,                   'http://jabber.org/protocol/geoloc').

% Defined by XEP-0083: Nested Roster Groups.
-define(NS_ROSTER_DELIMITER,         'roster:delimiter').

% Defined by XEP-0084: User Avatar.
-define(NS_USER_AVATAR,              'urn:xmpp:avatar:metadata').

% Defined by XEP-0085: Chat State Notifications
-define(NS_CHATSTATES,               'http://jabber.org/protocol/chatstates').

% Defined by XEP-0092: Software Version.
-define(NS_SOFT_VERSION,             'jabber:iq:version').

% Defined by XEP-0095: Stream Initiation.
-define(NS_SI,                       'http://jabber.org/protocol/si').

% Defined by XEP-0096: File Transfer.
-define(NS_FILE_TRANSFERT,
  'http://jabber.org/protocol/si/profile/file-transfer').

% Defined by XEP-0100: Gateway Interaction.
-define(NS_GATEWAY,                  'jabber:iq:gateway').

% Defined by XEP-0107: User Mood.
-define(NS_USER_MOOD,                'http://jabber.org/protocol/mood').

% Defined by XEP-0108: User Activity.
-define(NS_USER_ACTIVITY,            'http://jabber.org/protocol/activity').

% Defined by XEP-0114: Jabber Component Protocol.
-define(NS_COMPONENT_ACCEPT,         'jabber:component:accept').
-define(NS_COMPONENT_CONNECT,        'jabber:component:connect').

% Defined by XEP-0115: Entity Capabilities.
-define(NS_CAPS,                     'http://jabber.org/protocol/caps').

% Defined by XEP-0118: User Tune.
-define(NS_USER_TUNE,                'http://jabber.org/protocol/tune').

% Defined by XEP-0122: Data Forms Validation.
-define(NS_DATA_FORMS_VALIDATE,
  'http://jabber.org/protocol/xdata-validate').

% Defined by XEP-0124: Bidirectional-streams Over Synchronous HTTP.
-define(NS_BOSH,                     'http://jabber.org/protocol/httpbind').

% Defined by XEP-0130: Waiting Lists.
-define(NS_WAITING_LIST,             'http://jabber.org/protocol/waitinglist').

% Defined by XEP-0137: Publishing Stream Initiation Requests.
-define(NS_SI_PUB,                   'http://jabber.org/protocol/sipub').

% Defined by XEP-0138: Stream Compression.
-define(NS_COMPRESS,                 'http://jabber.org/protocol/compress').
-define(NS_COMPRESS_FEAT,            'http://jabber.org/features/compress').

% Defined by XEP-0141: Data Forms Layout.
-define(NS_DATA_FORMS_LAYOUT,
  'http://jabber.org/protocol/xdata-layout').

% Defined by XEP-0144: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE,          'http://jabber.org/protocol/rosterx').

% Defined by XEP-0145: Annotations.
-define(NS_ROSTER_NOTES,             'storage:rosternotes').

% Defined by XEP-0153: vCard-Based Avatars.
-define(NS_VCARD_UPDATE,             'vcard-temp:x:update').

% Defined by XEP-0171: Language Translation.
-define(NS_LANG_TRANS,               'urn:xmpp:langtrans').
-define(NS_LANG_TRANS_ITEMS,         'urn:xmpp:langtrans#items').

% Defined by XEP-0172: User Nickname.
-define(NS_USER_NICKNAME,            'http://jabber.org/protocol/nick').

% Defined by XEP-0184: Message Receipts.
-define(NS_RECEIPTS,                 'urn:xmpp:receipts').

% Defined by XEP-0191: Simple Communications Blocking.
-define(NS_BLOCKING,                 'urn:xmpp:blocking').
-define(NS_BLOCKING_ERRORS,          'urn:xmpp:blocking:errors').

% Defined by XEP-0199: XMPP Ping.
-define(NS_PING,                     'urn:xmpp:ping').

% Defined by XEP-0202: Entity Time.
-define(NS_TIME,                     'urn:xmpp:time').

% Defined by XEP-0203: Delayed Delivery.
-define(NS_DELAY,                    'urn:xmpp:delay').

% Defined by XEP-0206: XMPP Over BOSH.
-define(NS_XBOSH,                    'urn:xmpp:xbosh').

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
