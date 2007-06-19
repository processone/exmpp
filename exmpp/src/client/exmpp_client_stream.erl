% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_stream).
-vsn('$Revision$').

-export([
  opening/1,
  closing/0,
  closing/1,
  get_domain/1,
  get_version/1,
  get_lang/1
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

% --------------------------------------------------------------------
% Stream standard attributes.
% --------------------------------------------------------------------

get_domain(Opening) ->
    case exmpp_xml:get_attribute(Opening, 'to') of
        false -> false;
        ""    -> false;
        To    -> To
    end.

get_version(Opening) ->
    exmpp_stream:get_version(Opening).

get_lang(Opening) ->
    exmpp_stream:get_lang(Opening).
