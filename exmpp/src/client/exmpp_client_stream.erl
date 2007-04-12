% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_stream).
-vsn('$Revision$').

-export([
  opening/1,
  closing/0,
  closing/1
]).

% --------------------------------------------------------------------
% Stream opening/closing.
% --------------------------------------------------------------------

opening(Args) ->
    exmpp_stream:opening([{context, client} | Args]).

closing() ->
    exmpp_stream:closing().

closing(Opening) ->
    exmpp_stream:closing(Opening).
