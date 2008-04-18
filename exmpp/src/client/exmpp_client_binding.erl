% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the client side of
%% Resource Binding.

-module(exmpp_client_binding).
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature announcement.
-export([
  announced_support/1
]).

% Resource binding.
-export([
  bind/0,
  bind/1,
  bounded_jid/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> bool()
%%     Features_Announcement = exmpp_xml:xmlnselement()
%% @throws {resource_binding, announced_support, invalid_feature, Feature}
%% @doc Tell if the Resource Binding feature is supported.

announced_support(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_BIND, 'bind') of
        undefined -> true;
        Child     -> announced_support2(Child)
    end.

announced_support2(#xmlnselement{children = []}) ->
    true;
announced_support2(Feature) ->
    throw({resource_binding, announced_support, invalid_feature, Feature}).

% --------------------------------------------------------------------
% Resource binding.
% --------------------------------------------------------------------

%% @spec () -> Bind
%%     Bind = exmpp_xml:xmlnselement()
%% @doc Prepare a Resource Binding request.

bind() ->
    bind(undefined).

%% @spec (Resource) -> Bind
%%     Bind = exmpp_xml:xmlnselement()
%% @doc Prepare a Resource Binding request for the given `Resource'.

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
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "set"),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, bind_id()),
    #xmlnselement{
      ns = ?NS_JABBER_CLIENT,
      name = 'iq',
      attrs = Attrs2,
      children = [Bind]
    }.

%% @spec (Bind) -> Jid
%%     Bind = exmpp_xml:xmlnselement()
%%     Jid = string()
%% @throws {resource_binding, bounded_jid, no_jid, Iq} |
%%         {resource_binding, bounded_jid, bind_error, Condition}
%% @doc Extract the JID given by the server.

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
                            throw({resource_binding, bounded_jid, no_jid, Iq})
                    end;
                _ ->
                    throw({resource_binding, bounded_jid, no_jid, Iq})
            end;
        "error" ->
            Condition = exmpp_stanza:get_condition(Iq),
            throw({resource_binding, bounded_jid, bind_error, Condition})
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
