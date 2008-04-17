% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the server side of
%% legacy authentication found in Jabber, before XMPP 1.0.
%%
%% @see exmpp_client_legacy_auth.
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_server_legacy_auth).
-vsn('$Revision$').

-include("exmpp.hrl").

% Jabber Legacy authentication.
-export([
  fields/1,
  success/1,
  failure/2
]).

% --------------------------------------------------------------------
% Jabber Legacy authentication.
% --------------------------------------------------------------------

%% @spec (Request_IQ) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlnselement()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for advertising fields.

fields(Request_IQ) ->
    Username_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username',
      children = []},
    Password_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password',
      children = []},
    Digest_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest',
      children = []},
    Resource_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource',
      children = []},
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = [Username_El, Password_El, Digest_El, Resource_El]
    },
    exmpp_iq:result(Request_IQ, Query).

%% @spec (Request_IQ) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlnselement()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

success(Request_IQ) ->
    exmpp_iq:result(Request_IQ).

%% @spec (Request_IQ, Condition) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlnselement()
%%     Condition = not_authorized | conflict | not_acceptable
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

failure(Request_IQ, Condition) ->
    Code = case Condition of
        'not-authorized' -> "401";
        'conflict'       -> "409";
        'not-acceptable' -> "406"
    end,
    Error = exmpp_xml:set_attribute(
      exmpp_error:error(Request_IQ#xmlnselement.ns, Condition),
      'code', Code),
    exmpp_iq:error_without_original(Request_IQ, Error).
