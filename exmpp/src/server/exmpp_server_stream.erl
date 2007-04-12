% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_stream).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  opening/1,
  opening_reply/1,
  closing/0,
  closing/1,
  features/1
]).

% --------------------------------------------------------------------
% Stream opening/closing.
% --------------------------------------------------------------------

opening(Args) ->
    exmpp_stream:opening([{context, server} | Args]).

opening_reply(Opening_Or_Args) ->
    exmpp_stream:opening_reply(Opening_Or_Args).

closing() ->
    exmpp_stream:closing().

closing(Opening) ->
    exmpp_stream:closing(Opening).

% --------------------------------------------------------------------
% Features announcement.
% --------------------------------------------------------------------

features(Features) ->
    #xmlnselement{
      ns = ?NS_XMPP,
      name = 'features',
      children = Features
    }.
