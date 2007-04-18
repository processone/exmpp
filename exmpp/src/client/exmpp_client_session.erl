% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_session).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  establish/0,
  is_established/1
]).

% --------------------------------------------------------------------
% Session establishment.
% --------------------------------------------------------------------

establish() ->
    Session = #xmlnselement{
      ns = ?NS_JABBER_SESSION,
      name = 'session',
      children = []
    },
    Iq = #xmlnselement{
      ns = ?NS_JABBER_CLIENT,
      name = 'iq',
      children = [Session]
    },
    exmpp_xml:set_attributes(Iq, [
      {'type', "set"},
      {'id', session_id()}
    ]).

is_established(#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'} = Iq) ->
    case exmpp_xml:get_attribute(Iq, 'type') of
        "result" ->
            ok;
        "error" ->
            case exmpp_xml:get_element_by_name(Iq, 'error') of
                #xmlnselement{children =
                  [#xmlnselement{ns = ?NS_XMPP_STANZAS, name = Reason}]} ->
                    {error, Reason};
                _ ->
                    {error, undefined}
            end
    end.

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

%% @spec () -> Bind_ID
%%     Session_ID = string()
%% @doc Generate a random session establishment iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

session_id() ->
    "session-" ++ integer_to_list(random:uniform(65536 * 65536)).
