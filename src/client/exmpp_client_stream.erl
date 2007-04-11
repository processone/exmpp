% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_stream).
-vsn('$Revision$').

-export([
  stream_opening/1,
  stream_closing/0,
  stream_closing/1
]).

% --------------------------------------------------------------------
% Stream opening/closing.
% --------------------------------------------------------------------

stream_opening(Args) ->
    exmpp_stream:stream_opening([{context, client} | Args]).

stream_closing() ->
    exmpp_stream:stream_closing().

stream_closing(Opening) ->
    exmpp_stream:stream_closing(Opening).
