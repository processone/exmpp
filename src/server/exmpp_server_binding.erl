% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the server side of
%% Resource Binding.

-module(exmpp_server_binding).
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature annoucement.
-export([
  feature/0
]).

% Resource binding.
-export([
  wished_resource/1,
  bind/2,
  error/2
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec () -> Feature
%%     Feature = exmpp_xml:xmlnselement()
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link
%% exmpp_server_stream:features/1}.

feature() ->
    #xmlnselement{
      ns = ?NS_BIND,
      name = 'bind',
      children = []
    }.

% --------------------------------------------------------------------
% Resource binding.
% --------------------------------------------------------------------

%% @spec (Iq) -> Resource | undefined
%%     Iq = exmpp_xml:xmlnselement()
%%     REsource = string()
%% @throws {resource_binding, wished_resource, invalid_bind, Iq}
%% @doc Return the resource the client wants or `undefined' if he
%% doesn't ask for any.

wished_resource(#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'} = Iq) ->
    case exmpp_stanza:get_type(Iq) of
        "set" ->
            case exmpp_xml:get_element_by_name(Iq, ?NS_BIND, 'bind') of
                #xmlnselement{} = Bind ->
                    case exmpp_xml:get_element_by_name(Bind, 'resource') of
                        #xmlnselement{} = Resource ->
                            exmpp_xml:get_cdata(Resource);
                        _ ->
                            undefined
                    end;
                _ ->
                    throw({resource_binding, wished_resource,
                        invalid_bind, Iq})
            end;
        _ ->
            throw({resource_binding, wished_resource, invalid_bind, Iq})
    end;
wished_resource(Stanza) ->
    throw({resource_binding, wished_resource, invalid_bind, Stanza}).

%% @spec (Iq, Jid) -> Reply
%%     Iq = exmpp_xml:xmlnselement()
%%     Jid = exmpp_jid:jid()
%%     Reply = exmpp_xml:xmlnselement()
%% @doc Prepare a reply to `Iq' to inform the client of its final JID.

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
    exmpp_stanza:set_type(Iq1, "result").

%% @spec (Iq, Condition) -> Error_Iq
%%     Iq = exmpp_xml:xmlnselement()
%%     Condition = atom()
%%     Error_Iq = exmpp_xml:xmlnselement()
%% @doc Prepare an error reply to `Iq'.

error(Iq, Condition) ->
    exmpp_stanza:reply_with_error(Iq, Condition).
