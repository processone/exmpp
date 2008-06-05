% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of legacy authentication found in Jabber, before XMPP 1.0.
%%
%% @see exmpp_client_legacy_auth.
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_server_legacy_auth).
-vsn('$Revision$').

-include("exmpp.hrl").

% Creating stanza.
-export([
  fields/1,
  fields/2,
  success/1,
  failure/2
]).

% Accessing informations.
-export([
  get_credentials/1
]).

% --------------------------------------------------------------------
% Creating stanza.
% --------------------------------------------------------------------

%% @spec (Request_IQ) -> Fields_IQ
%%     Request_IQ = exmpp_xml:xmlnselement()
%%     Fields_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for advertising fields.
%%
%% Both authentication methods are proposed.

fields(Request_IQ) ->
    fields(Request_IQ, both).

%% @spec (Request_IQ, Auth) -> Fields_IQ
%%     Request_IQ = exmpp_xml:xmlnselement()
%%     Auth = plain | digest | both
%%     Fields_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for advertising fields.

fields(Request_IQ, Auth) when ?IS_IQ(Request_IQ) ->
    Username_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username',
      children = []},
    Password_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password',
      children = []},
    Digest_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest',
      children = []},
    Resource_El = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource',
      children = []},
    Children = case Auth of
        plain  -> [Username_El, Password_El, Resource_El];
        digest -> [Username_El, Digest_El, Resource_El];
        both   -> [Username_El, Password_El, Digest_El, Resource_El]
    end,
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = Children
    },
    exmpp_iq:result(Request_IQ, Query).

%% @spec (Password_IQ) -> Success_IQ
%%     Password_IQ = exmpp_xml:xmlnselement()
%%     Success_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

success(Password_IQ) when ?IS_IQ(Password_IQ) ->
    exmpp_iq:result(Password_IQ).

%% @spec (Password_IQ, Condition) -> Failure_IQ
%%     Password_IQ = exmpp_xml:xmlnselement()
%%     Condition = not_authorized | conflict | not_acceptable
%%     Failure_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

failure(Password_IQ, Condition) when ?IS_IQ(Password_IQ) ->
    Code = case Condition of
        'not-authorized' -> "401";
        'conflict'       -> "409";
        'not-acceptable' -> "406"
    end,
    Error = exmpp_xml:set_attribute(
      exmpp_stanza:error(Password_IQ#xmlnselement.ns, Condition),
      'code', Code),
    exmpp_iq:error_without_original(Password_IQ, Error).

% --------------------------------------------------------------------
% Accessing informations.
% --------------------------------------------------------------------

get_credentials(Password_IQ) when ?IS_IQ(Password_IQ) ->
    Request = exmpp_iq:get_request(Password_IQ),
    case Request of
        #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query', children = Children}
          when length(Children) == 3 ->
            get_credentials2(Children, {undefined, undefined, undefined});
        _ ->
            throw({legacy_auth, get_credentials, invalid_iq, Password_IQ})
    end.

get_credentials2(
  [#xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'} = Field | Rest],
  {_Username, Password, Resource}) ->
    Username = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, Password, Resource});
get_credentials2(
  [#xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password'} = Field | Rest],
  {Username, _Password, Resource}) ->
    Password = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, {plain, Password}, Resource});
get_credentials2(
  [#xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest'} = Field | Rest],
  {Username, _Password, Resource}) ->
    Password = unhex(exmpp_xml:get_cdata_as_list(Field)),
    get_credentials2(Rest, {Username, {digest, Password}, Resource});
get_credentials2(
  [#xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource'} = Field | Rest],
  {Username, Password, _Resource}) ->
    Resource = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, Password, Resource});
get_credentials2([Field | _Rest], _Credentials) ->
    throw({legacy_auth, get_credentials, invalid_field, Field});
get_credentials2([], {undefined, _Password, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'username'});
get_credentials2([], {_Username, undefined, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'password'});
get_credentials2([], {_Username, _Password, undefined}) ->
    throw({legacy_auth, get_credentials, missing_field, 'resource'});
get_credentials2([], Credentials) ->
    Credentials.

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

unhex("") ->
    [];
unhex(Digest) when length(Digest) rem 2 /= 0 ->
    throw({legacy_auth, get_credentials, invalid_digest, Digest});
unhex(Digest) ->
    unhex2(Digest, []).

unhex2([C1, C2 | Rest], Plain) ->
    I1 = hexchar_to_int(C1),
    I2 = hexchar_to_int(C2),
    I = (I1 bsl 4) + I2,
    unhex2(Rest, [I | Plain]);
unhex2([], Plain) ->
    lists:reverse(Plain).

hexchar_to_int($0) ->  0;
hexchar_to_int($1) ->  1;
hexchar_to_int($2) ->  2;
hexchar_to_int($3) ->  3;
hexchar_to_int($4) ->  4;
hexchar_to_int($5) ->  5;
hexchar_to_int($6) ->  6;
hexchar_to_int($7) ->  7;
hexchar_to_int($8) ->  8;
hexchar_to_int($9) ->  9;
hexchar_to_int($a) -> 10;
hexchar_to_int($b) -> 11;
hexchar_to_int($c) -> 12;
hexchar_to_int($d) -> 13;
hexchar_to_int($e) -> 14;
hexchar_to_int($f) -> 15;
hexchar_to_int(C)  -> throw({legacy_auth, get_credentials, invalid_digest, C}).
