% $Id$

% Namespace and prefix macros.
-include("exmpp_nss.hrl").

% Records to represent XML nodes.
-include("exmpp_xml.hrl").

% Records to represent XMPP/Jabber specific structures.
-include("exmpp_xmpp.hrl").

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
