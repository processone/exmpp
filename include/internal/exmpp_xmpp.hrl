% $Id$

% WARNING: This header is internal to Exmpp. DO NOT include it in your
% applications!

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

