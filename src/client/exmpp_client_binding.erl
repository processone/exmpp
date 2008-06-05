% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of Resource Binding.

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
        undefined -> false;
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
    exmpp_iq:set(?NS_JABBER_CLIENT, Bind, exmpp_internals:random_id("bind")).

%% @spec (Bind) -> Jid
%%     Bind = exmpp_xml:xmlnselement()
%%     Jid = string()
%% @throws {resource_binding, bounded_jid, invalid_bind, Stanza} |
%%         {resource_binding, bounded_jid, no_jid, IQ} |
%%         {resource_binding, bounded_jid, bind_error, Condition}
%% @doc Extract the JID given by the server.

bounded_jid(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'result' ->
            case exmpp_iq:get_result(IQ) of
                #xmlnselement{ns = ?NS_BIND, name = 'bind'} = Bind ->
                    case exmpp_xml:get_element_by_name(Bind,
                      ?NS_BIND, 'jid') of
                        #xmlnselement{} = Jid_El ->
                            Jid_S = exmpp_xml:get_cdata_as_list(Jid_El),
                            exmpp_jid:string_to_jid(Jid_S);
                        _ ->
                            throw({resource_binding, bounded_jid, no_jid, IQ})
                    end;
                _ ->
                    throw({resource_binding, bounded_jid, no_jid, IQ})
            end;
        'error' ->
            Condition = exmpp_stanza:get_condition(IQ),
            throw({resource_binding, bounded_jid, bind_error, Condition})
    end;
bounded_jid(Stanza) ->
    throw({resource_binding, bounded_jid, invalid_bind, Stanza}).
