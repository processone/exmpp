% $Id$

% WARNING: This header is internal to Exmpp. DO NOT include it in your
% applications!

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

% JID.
-record(jid, {
  full_jid,   
  lnode,
  ldomain,
  lresource
}).
%%      Instead of storing the tree raw components by separate, we keep the original
%%      JID in one chunk. 
%%      The intention is to save some memory. Most of the time we already have
%%      the raw JID (for example comming from a stanza), so we avoid the need of 
%%      allocating a new binary when serializing (we can return the original, see
%%      make_jid/4). 
%%      Also we allocate less pointers by keeping the JID in one chunk instead than 
%%      in its tree separate components. 
%%      Tradeoff: We consume more CPU and memory for accesing a single raw 
%%      component (see exmpp_jid:node/1, exmpp_jid:domain/1, 
%%      exmpp_jid:resource/1), but load tests performed on ejabberd so far indicates an 
%%      observable memory win when using this representation, with no significant CPU cost.
%%     
