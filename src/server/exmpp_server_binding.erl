%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of Resource Binding.

-module(exmpp_server_binding).
-vsn('$Revision$').

-include("exmpp.hrl").

%% Feature annoucement.
-export([
	 feature/0
	]).

%% Resource binding.
-export([
	 wished_resource/1,
	 bind/2,
	 error/2
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec () -> Feature
%%     Feature = exmpp_xml:xmlel()
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature() ->
    #xmlel{ns = ?NS_BIND,
	   name = 'bind'
	  }.

%% --------------------------------------------------------------------
%% Resource binding.
%% --------------------------------------------------------------------

%% @spec (IQ) -> Resource | undefined
%%     IQ = exmpp_xml:xmlel()
%%     Resource = string()
%% @throws {resource_binding, wished_resource, invalid_bind, IQ}
%% @doc Return the resource the client wants or `undefined' if he
%% doesn't ask for any.

wished_resource(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'set' ->
            case exmpp_iq:get_request(IQ) of
                #xmlel{ns = ?NS_BIND, name = 'bind'} = Bind ->
                    case exmpp_xml:get_element(Bind, ?NS_BIND, 'resource') of
                        #xmlel{} = Resource ->
                            exmpp_xml:get_cdata_as_list(Resource);
                        _ ->
                            undefined
                    end;
                _ ->
                    throw({resource_binding, wished_resource,
			   invalid_bind, IQ})
            end;
        _ ->
            throw({resource_binding, wished_resource, invalid_bind, IQ})
    end;
wished_resource(Stanza) ->
    throw({resource_binding, wished_resource, invalid_bind, Stanza}).

%% @spec (IQ, Jid) -> Reply
%%     IQ = exmpp_xml:xmlel()
%%     Jid = exmpp_jid:jid()
%%     Reply = exmpp_xml:xmlel()
%% @doc Prepare a reply to `IQ' to inform the client of its final JID.

bind(IQ, Jid) when ?IS_IQ(IQ) ->
    Jid_B = exmpp_jid:to_binary(Jid),
    El = #xmlel{ns = ?NS_BIND,
		name = 'jid'
	       },
    Children = [exmpp_xml:set_cdata(El, Jid_B)],
    Bind = #xmlel{ns = ?NS_BIND,
		  name = 'bind',
		  children = Children
		 },
    exmpp_iq:result(IQ, Bind).

%% @spec (IQ, Condition) -> Error_IQ
%%     IQ = exmpp_xml:xmlel()
%%     Condition = atom()
%%     Error_IQ = exmpp_xml:xmlel()
%% @doc Prepare an error reply to `IQ'.

error(IQ, Condition) when ?IS_IQ(IQ) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    exmpp_iq:error(IQ, Error).
