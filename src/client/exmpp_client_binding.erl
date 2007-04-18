% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_binding).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  announced_support/1,
  bind/0,
  bind/1,
  bounded_jid/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

announced_support(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_BIND, 'bind') of
        false -> none;
        Child -> announced_support(Child)
    end;
announced_support(#xmlnselement{ns = ?NS_BIND, name = 'bind',
  children = []}) ->
    ok;
announced_support(#xmlnselement{ns = ?NS_BIND, name = 'bind'}) ->
    invalid.

% --------------------------------------------------------------------
% Resource binding.
% --------------------------------------------------------------------

bind() ->
    bind(undefined).

bind(Resource) ->
    Children = case Resource of
        undefined ->
            [];
        "" ->
            [];
        _ ->
            El = #xmlnselement{
              ns = ?NS_BIND,
              name = 'resource',
              children = []
            },
            [exmpp_xml:set_cdata(El, Resource)]
    end,
    Bind = #xmlnselement{
      ns = ?NS_BIND,
      name = 'bind',
      children = Children
    },
    Iq = #xmlnselement{
      ns = ?NS_JABBER_CLIENT,
      name = 'iq',
      children = [Bind]
    },
    exmpp_xml:set_attributes(Iq, [
      {'type', "set"},
      {'id', bind_id()}
    ]).

bounded_jid(#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'} = Iq) ->
    case exmpp_xml:get_attribute(Iq, 'type') of
        "result" ->
            case exmpp_xml:get_element_by_name(Iq, ?NS_BIND, 'bind') of
                #xmlnselement{} = Bind_El ->
                    case exmpp_xml:get_element_by_name(Bind_El, 'jid') of
                        #xmlnselement{} = Jid_El ->
                            Jid_S = exmpp_xml:get_cdata(Jid_El),
                            exmpp_jid:string_to_jid(Jid_S);
                        _ ->
                            {error, no_jid}
                    end;
                _ ->
                    {error, no_jid}
            end;
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
%%     Bind_ID = string()
%% @doc Generate a random resource binding iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

bind_id() ->
    "bind-" ++ integer_to_list(random:uniform(65536 * 65536)).
