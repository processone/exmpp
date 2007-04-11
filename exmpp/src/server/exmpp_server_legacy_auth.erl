% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @see exmpp_client_legacy_auth.

-module(exmpp_server_legacy_auth).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  fields/1,
  success/1,
  failure/2
]).

% --------------------------------------------------------------------
% Jabber Legacy authentication.
% --------------------------------------------------------------------

%% @spec (Id) -> Auth_Iq
%%     Id = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for advertising fields.

fields(Id) ->
    % Prepare fields.
    Username = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username',
      children = []},
    Password = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password',
      children = []},
    Digest = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest',
      children = []},
    Resource = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource',
      children = []},
    % Make query.
    Query = exmpp_xml:append_children(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query'},
      [Username, Password, Digest, Resource]),
    % Make IQ.
    Iq = exmpp_xml:set_attributes(
      #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
        {'type', "result"},
        {'id',   Id}
      ]),
    exmpp_xml:append_child(Iq, Query).

%% @spec (Id) -> Auth_Iq
%%     Id = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

success(Id) ->
    exmpp_xml:set_attributes(
      #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq', children = []}, [
        {'type', "result"},
        {'id', Id}
      ]).

%% @spec (Id, Reason) -> Auth_Iq
%%     Id = string()
%%     Reason = not_authorized | conflict | not_acceptable
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

failure(Id, Reason) ->
    % Make <error>.
    Error0 = #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'error'},
    Error = case Reason of
        not_authorized ->
            Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
              name = 'not-authorized', children = []},
            exmpp_xml:append_child(
              exmpp_xml:set_attributes(Error0, [
                {'code', "401"},
                {'type', "auth"}]), Child);
          conflict ->
              Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
                name = 'conflict', children = []},
              exmpp_xml:append_child(
                exmpp_xml:set_attributes(Error0, [
                  {'code', "409"},
                  {'type', "cancel"}]), Child);
          not_acceptable ->
              Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
                name = 'not-acceptable', children = []},
              exmpp_xml:append_child(
                exmpp_xml:set_attributes(Error0, [
                  {'code', "406"},
                  {'type', "modify"}]), Child)
    end,
    % Make IQ.
    Iq = exmpp_xml:set_attributes(
      #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
        {'type', "result"},
        {'id', Id}
      ]),
    exmpp_xml:append_child(Iq, Error).
