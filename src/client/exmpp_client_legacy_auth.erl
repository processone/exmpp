% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the client side of
%% legacy authentication found in Jabber, before XMPP 1.0.
%%
%% <p>
%% A common use case is presented in <em>table 1</em>.
%% </p>
%% <table class="illustration">
%% <caption>Table 1: stream opening and closing</caption>
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% Once a stream is opened, the client call `{@module}':
%% </p>
%% <pre>Request = exmpp_client_legacy_auth:request("jabber.example.com").</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq type="get" to="jabber.example.com" id="auth-1905181425"&gt;
%%   &lt;query xmlns="jabber:iq:auth"/&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server answer with the available fields:
%% </p>
%% <pre>Request_Id = exmpp_xml:get_attribute(Request, 'id'),<br/>Fields = exmpp_server_legacy_auth:fields(Request_Id).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-1905181425"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username/&gt;
%%               &lt;password/&gt;
%%               &lt;digest/&gt;
%%               &lt;resource/&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% <p>
%% At this time, this function doesn't offer the possibility to choose
%% which field to include (one may not want to propose `<password/>' for
%% instance). And because {@link exmpp_xml} doesn't provide a function
%% to remove element yet, the only way to achieve this is to walk
%% through the children and remove it by hand.
%% </p>
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client can send its credentials; he choose `<digest/>':
%% </p>
%% <pre>Password = exmpp_client_legacy_auth:password_digest(
%%   "johndoe",
%%   "foobar!",
%%   "home"<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;q xmlns="jabber:client" type="set" id="auth-3105434037"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username&gt;johndoe&lt;/username&gt;
%%               &lt;digest&gt;
%%                       93fdad2a795c59c73a6acf68a4dbdd3ddb366239
%%               &lt;/digest&gt;
%%               &lt;resource&gt;home&lt;/resource&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% If the password is correct, the server notify the client:
%% </p>
%% <pre>Password_Id = exmpp_xml:get_attribute(Password, 'id'),<br/>Success = exmpp_server_legacy_auth:success(Password_Id).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-3105434037"/&gt;</pre>
%% </td>
%% </tr>
%% </table>
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_client_legacy_auth).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  request/1,
  request/2,
  request_with_user/2,
  request_with_user/3,
  password/3,
  password/4,
  password_digest/3,
  password_digest/4
]).

% --------------------------------------------------------------------
% Jabber Legacy authentication.
% --------------------------------------------------------------------

%% @spec (To) -> Response_IQ
%%     To = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

request(To) ->
    request(To, auth_id()).

%% @spec (To, ID) -> Response_IQ
%%     To = string()
%%     ID = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.

request(To, ID) ->
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = []
    },
    IQ = exmpp_iq:get(?NS_JABBER_CLIENT, Query, ID),
    exmpp_error:set_recipient(IQ, To).

%% @spec (To, Username) -> Response_IQ
%%     To = string()
%%     Username = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

request_with_user(To, Username) ->
    request_with_user(To, Username, auth_id()).

%% @spec (To, Username, ID) -> Response_IQ
%%     To = string()
%%     Username = string()
%%     ID = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.

request_with_user(To, Username, ID) ->
    Username_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
      Username),
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = [Username_El]
    },
    IQ = exmpp_iq:get(?NS_JABBER_CLIENT, Query, ID),
    exmpp_error:set_recipient(IQ, To).

%% @spec (Username, Password, Resource) -> Response_IQ
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

password(Username, Password, Resource) ->
    password(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Response_IQ
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     ID = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is in clear plain text in the stanza.
%%
%% For an anonymous authentication, `Password' may be the empty string.

password(Username, Password, Resource, ID) ->
    Username_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
      Username),
    Password_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password'},
      Password),
    Resource_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource'},
      Resource),
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = [Username_El, Password_El, Resource_El]
    },
    exmpp_iq:set(?NS_JABBER_CLIENT, Query, ID).

%% @spec (Username, Password, Resource) -> Response_IQ
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

password_digest(Username, Password, Resource) ->
    password_digest(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Response_IQ
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     ID = string()
%%     Response_IQ = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is encoded as specified in XEP-0078.

password_digest(Username, Password, Resource, ID) ->
    Username_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
      Username),
    Digest_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest'},
      password_digest(ID, Password)),
    Resource_El = exmpp_xml:set_cdata(
      #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource'},
      Resource),
    Query = #xmlnselement{
      ns = ?NS_JABBER_AUTH,
      name = 'query',
      children = [Username_El, Digest_El, Resource_El]
    },
    exmpp_iq:set(?NS_JABBER_CLIENT, Query, ID).

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

%% @spec () -> Auth_ID
%%     Auth_ID = string()
%% @doc Generate a random authentication iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

auth_id() ->
    "auth-" ++ integer_to_list(random:uniform(65536 * 65536)).

%% @spec (Id, Passwd) -> Digest
%%     Id = string()
%%     Passwd = string()
%%     Digest = string()
%% @doc Produce a password digest for legacy auth, according to XEP-0078.

password_digest(Id, Passwd) ->
    Token = Id ++ Passwd,
    crypto:start(),
    hex(binary_to_list(crypto:sha(Token))).

hex(L) when is_list(L) ->
    lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
    [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I) ->
    [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I)  -> $0 +I.
