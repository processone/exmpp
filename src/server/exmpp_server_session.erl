%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of the Session Establishment.

-module(exmpp_server_session).
-vsn('$Revision$').

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 feature/0
	]).

%% Session establishment.
-export([
	 want_establishment/1,
	 establish/1,
	 error/2
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec () -> Feature
%%     Feature = exmpp_xml:xmlel()
%% @doc Make a feature annoucement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature() ->
    #xmlel{ns = ?NS_SESSION,
	   name = 'session'
	  }.

%% --------------------------------------------------------------------
%% Session establishment.
%% --------------------------------------------------------------------

%% @spec (IQ) -> bool()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the initiating entity wants to establish a session.

want_establishment(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'set' ->
            case exmpp_iq:get_request(IQ) of
                #xmlel{ns = ?NS_SESSION, name = 'session'} -> true;
                _                                          -> false
            end;
        _ ->
            false
    end;
want_establishment(_Stanza) ->
    false.

%% @spec (IQ) -> Result_IQ
%%     IQ = exmpp_xml:xmlel()
%%     Result_IQ = exmpp_xml:xmlel()
%% @doc Prepare a result IQ to inform the initiating entity that the
%% session is created.

establish(IQ) when ?IS_IQ(IQ) ->
    exmpp_iq:result(IQ).

%% @spec (IQ, Condition) -> Error_IQ
%%     IQ = exmpp_xml:xmlel()
%%     Condition = atom()
%%     Error_IQ = exmpp_xml:xmlel()
%% @doc Prepare an error reply to `IQ'.

error(IQ, Condition) when ?IS_IQ(IQ) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    exmpp_iq:error(IQ, Error).
