% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of the Session Establishment.

-module(exmpp_client_session).
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature announcement.
-export([
  announced_support/1
]).

% Session establishment.
-export([
  establish/0,
  check_establishment/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> bool()
%%     Features_Announcement = exmpp_xml:xmlnselement()
%% @throws {session, announced_support, invalid_feature, Feature}
%% @doc Tell if the Session Establishment feature is supported.

announced_support(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_SESSION, 'session') of
        undefined -> false;
        Child     -> announced_support2(Child)
    end.    
    
announced_support2(#xmlnselement{children = []}) ->
    true;
announced_support2(Feature) ->
    throw({session, announced_support, invalid_feature, Feature}).

% --------------------------------------------------------------------
% Session establishment.
% --------------------------------------------------------------------

%% @spec () -> Session
%%     Session = exmpp_xml:xmlnselement()
%% @doc Make a `<session/>' element to create a session.

establish() ->
    Session = #xmlnselement{
      ns = ?NS_SESSION,
      name = 'session',
      children = []
    },
    exmpp_iq:set(?NS_JABBER_CLIENT, Session,
      exmpp_internals:random_id("session")).

%% @spec (IQ) -> ok
%%     IQ = exmpp_xml:xmlnselement()
%% @throws {session, check_establishment, establishment_failed, Condition}
%% @doc Check that the session was created successfully.

check_establishment(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'result' ->
            ok;
        'error' ->
            throw({session, check_establishment,
              establishment_failed, exmpp_stanza:get_condition(IQ)})
    end.
