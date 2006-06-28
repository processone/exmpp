% $Id$

% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Elements without namespace support.
-record(xmlelement, {
	name,		% Tag name
	attrs = [],	% Attribute list
	children = []	% Children (tags or CDATA)
}).

-record(xmlendelement, {
	name
}).

% Elements WITH namespace support.
-record(xmlnselement, {
	ns = undefined,	    % Tag namespace
	prefix = undefined, % Namespace prefix
	name,		    % Tag name
	attrs = [],	    % Attribute list
	children = []	    % Children (tags or CDATA)
}).

-record(xmlnsendelement, {
	ns = undefined,
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
	cdata = []	% Character data
}).

% --------------------------------------------------------------------
% Records to represent events.
% --------------------------------------------------------------------

% Stream start.
-record(xmlstreamstart, {
	name,		% Name of the tag that opened the stream
	attrs = []	% Attribute list
}).

% Depth 1 element, inside a stream.
-record(xmlstreamelement, {
	element		% #xmlelement
}).

% Stream end.
-record(xmlstreamend, {
	name		% Tag name
}).

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

-record(jid, {
	user,
	server,
	resource,
	luser,
	lserver,
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
% Defines for exmpp_jlib.
% --------------------------------------------------------------------

-define(NS_DELAY, "jabber:x:delay").

% vim:ft=erlang:
