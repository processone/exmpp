% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_binding).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  feature/0,
  wished_resource/1,
  bind/2,
  error/2
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

feature() ->
    #xmlnselement{
      ns = ?NS_BIND,
      name = 'bind',
      children = []
    }.

% --------------------------------------------------------------------
% Resource binding.
% --------------------------------------------------------------------

wished_resource(#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'} = Iq) ->
    case exmpp_xml:get_attribute(Iq, 'type') of
        "set" ->
            case exmpp_xml:get_element_by_name(Iq, ?NS_BIND, 'bind') of
                #xmlnselement{} = Bind ->
                    case exmpp_xml:get_element_by_name(Bind, 'resource') of
                        #xmlnselement{} = Resource ->
                            {ok, exmpp_xml:get_cdata(Resource)};
                        _ ->
                            {ok, none}
                    end;
                _ ->
                    {error, no_bind}
            end;
        _ ->
            {error, unexpected_iq}
    end.

bind(Iq, Jid) ->
    Jid_S = exmpp_jid:jid_to_string(Jid),
    El = #xmlnselement{
      ns = ?NS_BIND,
      name = 'jid',
      children = []
    },
    Children = [exmpp_xml:set_cdata(El, Jid_S)],
    Bind = #xmlnselement{
      ns = ?NS_BIND,
      name = 'bind',
      children = Children
    },
    Iq1 = exmpp_xml:set_children(Iq, [Bind]),
    exmpp_xml:set_attribute(Iq1, 'type', "result").

error(Iq, Reason) ->
    El = #xmlnselement{
      ns = ?NS_XMPP_STANZAS,
      name = Reason,
      children = []
    },
    Error0 = #xmlnselement{
      ns = ?NS_JABBER_CLIENT,
      name = 'error',
      children = [El]
    },
    Error = case Reason of
        'bad-request' ->
            exmpp_xml:set_attribute(Error0, 'type', "modify");
        _ ->
            exmpp_xml:set_attribute(Error0, 'type', "cancel")
    end,
    Iq1 = exmpp_xml:append_child(Iq, Error),
    exmpp_xml:set_attribute(Iq1, 'type', "result").
