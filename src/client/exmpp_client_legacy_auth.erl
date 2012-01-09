%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of legacy authentication found in Jabber, before XMPP
%% 1.0.
%%
%% <p>
%% This table presents a successful legacy authentication.
%% </p>
%% <table class="illustration">
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
%% <pre>Fields = exmpp_server_legacy_auth:fields(Request).</pre>
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
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client can send its credentials:
%% </p>
%% <pre>Password = exmpp_client_legacy_auth:password(
%%   Fields,
%%   "johndoe",
%%   "foobar!",
%%   "home"<br/>).</pre>
%% <p>
%% The best method is chosen automatically (here, `<digest/>').
%% </p>
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
%% <pre>Success = exmpp_server_legacy_auth:success(Password).</pre>
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

-include("exmpp.hrl").

%% Creating stanza.
-export([
    request/1,
    request/2,
    request_with_user/2,
    request_with_user/3,
    password/4,
    password/5,
    password_plain/3,
    password_plain/4,
    password_digest/3,
    password_digest/4
]).

%% Accessing informations.
-export([
    get_fields/1,
    get_prefered_auth/1,
    is_success/1
]).

%% Tools.
-export([
	 digest/2,
	 hex/1
	]).

-export_type([
    username/0,
    password/0,
    resource/0,
    digest/0
]).

-type(username() :: binary()).
-type(password() :: binary()).
-type(resource() :: binary()).
-type(digest()   :: binary()).

-export_type([
    auth/0,
    auth_digest/0,
    auth_plain/0
]).

%% <<"digest">>
-type(auth_digest() :: <<_:48>>).
%% <<"plain">>
-type(auth_plain() :: <<_:40>>).

-type(auth() :: exmpp_client_legacy_auth:auth_digest()
              | exmpp_client_legacy_auth:auth_plain()
).


%%
-define(Xmlel@LEGACY_AUTH(Name, Attrs, Children),
(
    exxml:element(?NS_LEGACY_AUTH, Name, Attrs, Children)
%    #xmlel{
%        name     = Name,
%        attrs    = [{<<"xmlns">>, ?NS_LEGACY_AUTH} | Attrs],
%        children = Children
%    }
)).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (To) -> Request_IQ
%%     To = binary()
%%     Request_IQ = exxml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

-spec(request/1 ::
(
  To::exmpp_stanza:to())
    -> Stanza_IQ_Get::exmpp_stanza:stanza_get()
).

request(To) ->
    request(To, auth_id()).

%% @spec (To, ID) -> Request_IQ
%%     To = binary()
%%     ID = binary()
%%     Request_IQ = exxml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.

-spec(request/2 ::
(
  To::exmpp_stanza:to(),
  Id::exmpp_stanza:id())
    -> Stanza_IQ_Get::exmpp_stanza:stanza_get()
).

request(To, Id) ->
    exmpp_stanza:set_recipient(
        exmpp_iq:get(?Xmlel@LEGACY_AUTH(<<"query">>, [], []), Id),
        To).

%% @spec (To, Username) -> Request_IQ
%%     To = binary()
%%     Username = binary()
%%     Request_IQ = exxml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

-spec(request_with_user/2 ::
(
  To       :: exmpp_stanza:to(),
  Username :: exmpp_client_legacy_auth:username())
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

request_with_user(To, Username) ->
    request_with_user(To, Username, auth_id()).

%% @spec (To, Username, ID) -> Request_IQ
%%     To = binary()
%%     Username = binary()
%%     ID = binary()
%%     Response_IQ = exxml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.

-spec(request_with_user/3 ::
(
  To       :: exmpp_stanza:to(),
  Username :: exmpp_client_legacy_auth:username(),
  Id       :: exmpp_stanza:id())
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

request_with_user(To, Username, Id) ->
    exmpp_stanza:set_recipient(
        exmpp_iq:get(
            ?Xmlel@LEGACY_AUTH(<<"query">>, [], [
                ?Xmlel@LEGACY_AUTH(<<"username">>, [], [exxml:cdata(Username)])
                ]),
            Id),
        To).

%% @spec (Fields_IQ, Username, Password, Resource) -> Password_IQ
%%     Fields_IQ = exxml:xmlel()
%%     Username = binary()
%%     Password = binary() | nil()
%%     Resource = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq/>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

-spec(password/4 ::
(
  Stanza_IQ_Result :: exmpp_stanza:iq_result(),
  Username         :: exmpp_client_legacy_auth:username(),
  Password         :: exmpp_client_legacy_auth:password(),
  Resource         :: exmpp_client_legacy_auth:resource())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password(Stanza_IQ_Result, Username, Password, Resource) ->
    password(Stanza_IQ_Result, Username, Password, Resource, auth_id()).

%% @spec (Fields_IQ, Username, Password, Resource, ID) -> Password_IQ
%%     Fields_IQ = exxml:xmlel()
%%     Username = binary()
%%     Password = binary() | nil()
%%     Resource = binary()
%%     ID = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq/>' to send authentication informations.

-spec(password/5 ::
(
  Stanza_IQ_Result :: exmpp_stanza:iq_result(),
  Username         :: exmpp_client_legacy_auth:username(),
  Password         :: exmpp_client_legacy_auth:password(),
  Resource         :: exmpp_client_legacy_auth:resource(),
  Id               :: exmpp_stanza:id())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password(Stanza_IQ_Result, Username, Password, Resource, Id) ->
    case get_prefered_auth(Stanza_IQ_Result) of
        <<"plain">>  -> password_plain(Username, Password, Resource, Id);
        <<"digest">> -> password_digest(Username, Password, Resource, Id)
    end.

%% @spec (Username, Password, Resource) -> Password_IQ
%%     Username = binary()
%%     Password = binary() | nil()
%%     Resource = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

-spec(password_plain/3 ::
(
  Username :: exmpp_client_legacy_auth:username(),
  Password :: exmpp_client_legacy_auth:password(),
  Resource :: exmpp_client_legacy_auth:resource())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password_plain(Username, Password, Resource) ->
    password_plain(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Password_IQ
%%     Username = binary()
%%     Password = binary() | nil()
%%     Resource = binary()
%%     ID = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is in clear plain text in the stanza.
%%
%% For an anonymous authentication, `Password' may be the empty string.

-spec(password_plain/4 ::
(
  Username :: exmpp_client_legacy_auth:username(),
  Password :: exmpp_client_legacy_auth:password(),
  Resource :: exmpp_client_legacy_auth:resource(),
  Id       :: exmpp_stanza:id())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password_plain(Username, Password, Resource, Id) ->
    exmpp_iq:set(
        ?Xmlel@LEGACY_AUTH(<<"query">>, [], [
            ?Xmlel@LEGACY_AUTH(<<"username">>, [], [exxml:cdata(Username)]),
            ?Xmlel@LEGACY_AUTH(<<"password">>, [], [exxml:cdata(Password)]),
            ?Xmlel@LEGACY_AUTH(<<"resource">>, [], [exxml:cdata(Resource)])
        ]),
        Id).

%% @spec (Username, Password, Resource) -> Password_IQ
%%     Username = binary()
%%     Password = binary()
%%     Resource = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

-spec(password_digest/3 ::
(
  Username :: exmpp_client_legacy_auth:username(),
  Password :: exmpp_client_legacy_auth:password(),
  Resource :: exmpp_client_legacy_auth:resource())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password_digest(Username, Password, Resource) ->
    password_digest(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Password_IQ
%%     Username = binary()
%%     Password = binary()
%%     Resource = binary()
%%     ID = binary()
%%     Password_IQ = exxml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is encoded as specified in XEP-0078.

-spec(password_digest/4 ::
(
  Username :: exmpp_client_legacy_auth:username(),
  Password :: exmpp_client_legacy_auth:password(),
  Resource :: exmpp_client_legacy_auth:resource(),
  Id       :: exmpp_stanza:id())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

password_digest(Username, Password, Resource, Id) ->
    exmpp_iq:set(
        ?Xmlel@LEGACY_AUTH(<<"query">>, [], [
            ?Xmlel@LEGACY_AUTH(<<"username">>, [], [exxml:cdata(Username)]),
            ?Xmlel@LEGACY_AUTH(<<"digest">>, [],
                [exxml:cdata(hex(digest(Id, Password)))]),
            ?Xmlel@LEGACY_AUTH(<<"resource">>, [], [exxml:cdata(Resource)])
        ]),
        Id).

%% --------------------------------------------------------------------
%% Accessing informations.
%% --------------------------------------------------------------------

%% @spec (Fields_IQ) -> Fields
%%     Fields_IQ = exxml:xmlel()
%%     Fields = [binary()]
%% @throws {legacy_auth, get_fields, invalid_iq, Fields_IQ} |
%%         {legacy_auth, get_fields, invalid_field, Field}
%% @doc Return the list of fields supported by the server.

-spec(get_fields/1 ::
(
  Stanza_IQ_Result :: exmpp_stanza:iq_result())
    -> Fields :: [Field::binary()]
).

get_fields(Stanza_IQ_Result) when ?IS_IQ(Stanza_IQ_Result) ->
    case exmpp_iq:get_result(Stanza_IQ_Result) of
        undefined ->
            throw({legacy_auth, get_fields, invalid_iq, Stanza_IQ_Result});
        #xmlel{name = <<"query">>, children = Children}
          when length(Children) == 3 orelse length(Children) == 4 ->
            get_fields2(Children, []);
        _ ->
            throw({legacy_auth, get_fields, invalid_iq, Stanza_IQ_Result})
    end.

%%
-spec(get_fields2/2 ::
(
  Xmlels :: exxml:els(),
  Fields :: [Field::binary()])
    -> Fields :: [Field::binary()]
).

get_fields2([#xmlel{name = Field} | Xmlels], Fields) ->
    get_fields2(Xmlels, [Field | Fields]);
get_fields2([Xmlel | _Xmlels], _Fields) ->
    throw({legacy_auth, get_fields, invalid_field, Xmlel});
get_fields2([], Fields) ->
    lists:reverse(Fields).

%% @spec (Fields_IQ) -> Auth
%%     Fields_IQ = exxml:xmlel()
%%     Auth = <<"digest">> | <<"password">>
%% @doc Return the prefered authentication method.

-spec(get_prefered_auth/1 ::
(
  Stanza_IQ_Result :: exmpp_stanza:iq_result())
    -> Auth::exmpp_client_legacy_auth:auth()
).

get_prefered_auth(Stanza_IQ_Result) when ?IS_IQ(Stanza_IQ_Result) ->
    case lists:member(<<"digest">>, get_fields(Stanza_IQ_Result)) of
        true -> <<"digest">>;
        _    -> <<"plain">>
    end.

%% @spec (IQ) -> bool()
%%     IQ = exxml:xmlel()
%% @doc Tell if the authentication succeeded.

-spec(is_success/1 ::
(
  Stanza_IQ :: exmpp_stanza:iq_result() | exmpp_stanza:iq_error())
    -> Is_Success::boolean()
).

is_success(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case exmpp_iq:get_type(Stanza_IQ) of
        <<"result">> -> true;
        <<"error">>  -> false;
        _        -> throw({legacy_auth, is_success, unexpected_iq, Stanza_IQ})
    end.

%% --------------------------------------------------------------------
%% Tools.
%% --------------------------------------------------------------------

%% @spec (ID, Passwd) -> Digest
%%     ID = binary()
%%     Passwd = binary()
%%     Digest = binary()
%% @doc Produce a password digest for legacy auth, according to XEP-0078.

-spec(digest/2 ::
(
  Id       :: exmpp_stanza:id(),
  Password :: exmpp_client_legacy_auth:password())
    -> Digest::exmpp_client_legacy_auth:digest()
).

digest(Id, Password) ->
    Token = <<Id/binary, Password/binary>>,
    crypto:start(),
    crypto:sha(Token).

%% @spec (Plain) -> Hex
%%     Plain = binary()
%%     Hex = binary()
%% @doc Encode list to a hexadecimal string.

hex(Plain) ->
    iolist_to_binary([hex2(I) || I <- binary_to_list(Plain)]).

hex2(I) when I > 16#f ->
    [int_to_hexchar((I band 16#f0) bsr 4), int_to_hexchar((I band 16#0f))];
hex2(I) ->
    [$0, int_to_hexchar(I)].

int_to_hexchar(10) -> $a;
int_to_hexchar(11) -> $b;
int_to_hexchar(12) -> $c;
int_to_hexchar(13) -> $d;
int_to_hexchar(14) -> $e;
int_to_hexchar(15) -> $f;
int_to_hexchar(I)  -> $0 + I.

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

%% @spec () -> Auth_ID
%%     Auth_ID = string()
%% @doc Generate a random authentication iq ID.
%%
%% @see exmpp_utils:random_id/1.

-spec(auth_id/0 :: () -> Id::exmpp_stanza:id()).

auth_id() ->
    exmpp_utils:random_id(<<"auth-">>).
