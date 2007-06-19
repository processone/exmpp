% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_session).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  feature/0,
  want_establishment/1,
  establish/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

feature() ->
    #xmlnselement{
      ns = ?NS_JABBER_SESSION,
      name = 'session',
      children = []
    }.

% --------------------------------------------------------------------
% Session establishment.
% --------------------------------------------------------------------

want_establishment(#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'} = Iq) ->
    case exmpp_xml:get_attribute(Iq, 'type') of
        "set" ->
            case exmpp_xml:get_element_by_name(Iq,
              ?NS_JABBER_SESSION, 'session') of
                #xmlnselement{} ->
                    ok;
                _ ->
                    {error, unexpected_stanza}
            end;
        _ ->
            {error, unexpected_stanza}
    end;
want_establishment(#xmlnselement{}) ->
    {error, unexpected_stanza}.

establish(Iq) ->
    Iq1 = exmpp_xml:set_children(Iq, []),
    exmpp_xml:set_attribute(Iq1, 'type', "result").
