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
  get_domain/1,
  get_version/1,
  get_id/1,
  get_lang/1,
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
% Stream standard attributes.
% --------------------------------------------------------------------

get_domain(Opening) ->
    case exmpp_xml:get_attribute(Opening, 'from') of
        false -> false;
        ""    -> false;
        From  -> From
    end.

get_version(Opening) ->
    exmpp_stream:get_version(Opening).

get_id(Opening) ->
    case exmpp_xml:get_attribute(Opening, 'id') of
        false -> false;
        Id    -> Id
    end.

get_lang(Opening) ->
    exmpp_stream:get_lang(Opening).

% --------------------------------------------------------------------
% Features announcement.
% --------------------------------------------------------------------

features(Features) ->
    #xmlnselement{
      ns = ?NS_XMPP,
      prefix = "stream",
      name = 'features',
      children = Features
    }.
