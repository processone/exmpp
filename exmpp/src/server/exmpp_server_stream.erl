% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_stream).
-vsn('$Revision$').

-export([
  stream_opening/1,
  stream_opening_reply/1,
  stream_closing/0,
  stream_closing/1
]).

% --------------------------------------------------------------------
% Stream opening/closing.
% --------------------------------------------------------------------

stream_opening(Args) ->
    exmpp_stream:stream_opening([{context, server} | Args]).

stream_opening_reply(Opening_Or_Args) ->
    exmpp_stream:stream_opening_reply(Opening_Or_Args).

stream_closing() ->
    exmpp_stream:stream_closing().

stream_closing(Opening) ->
    exmpp_stream:stream_closing(Opening).
