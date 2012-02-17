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
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of legacy authentication found in Jabber, before XMPP 1.0.
%%
%% @see exmpp_client_legacy_auth.
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_server_legacy_auth).

-include("exmpp.hrl").

%% Creating stanza.
-export([
    fields/1,
    fields/2,
    success/1,
    failure/2
]).

%% Accessing informations.
-export([
    want_fields/1,
    get_credentials/1
]).

%% Tools.
-export([
    unhex/1
]).

%%
-export_type([
    error_condition/0
]).

-type(error_condition() :: binary()).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@Legacy_Auth(Name, Attrs, Children),
(
    exxml:element(?NS_LEGACY_AUTH, Name, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @doc Make an `<iq>' for advertising fields.
%%
%% Both authentication methods are proposed.
-spec(fields/1 ::
(
  Stanza_IQ_Set::exmpp_stanza:iq_set())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result()
).

fields(Request_IQ) ->
    fields(Request_IQ, 'both').

%% @doc Make an `<iq>' for advertising fields.
-spec(fields/2 ::
(
  Stanza_IQ_Set :: exmpp_stanza:iq_set(),
  Auth_Method   :: 'plain' | 'digest' | 'both')
    -> Stanza_IQ_Result::exmpp_stanza:iq_result()
).

fields(Stanza_IQ_Set, 'plain' = _Auth_Method) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:result(Stanza_IQ_Set,
        ?Xmlel@Legacy_Auth(<<"query">>, [], [
            ?Xmlel(<<"username">>, [],
                case
                    exxml:get_path(Stanza_IQ_Set,
                        [{'element', <<"query">> },
                         {'element', <<"username">>},
                         'cdata'
                        ])
                of
                    <<>>     -> [];
                    Username -> [exxml:cdata(Username)]
                end),
            ?Xmlel(<<"password">>, [], []),
            ?Xmlel(<<"resource">>, [], [])
        ])
    );
fields(Stanza_IQ_Set, 'digest' = _Auth_Method) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:result(Stanza_IQ_Set,
        ?Xmlel@Legacy_Auth(<<"query">>, [], [
            ?Xmlel(<<"username">>, [],
                case
                    exxml:get_path(Stanza_IQ_Set,
                        [{'element', <<"query">> },
                         {'element', <<"username">>},
                         'cdata'
                        ])
                of
                    <<>>     -> [];
                    Username -> [exxml:cdata(Username)]
                end),
            ?Xmlel(<<"digest">>, [], []),
            ?Xmlel(<<"resource">>, [], [])
        ])
    );
fields(Stanza_IQ_Set, 'both' = _Auth_Method) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:result(Stanza_IQ_Set,
        ?Xmlel@Legacy_Auth(<<"query">>, [], [
            ?Xmlel(<<"username">>, [],
                case
                    exxml:get_path(Stanza_IQ_Set,
                        [{'element', <<"query">> },
                         {'element', <<"username">>},
                         'cdata'
                        ])
                of
                    <<>>     -> [];
                    Username -> [exxml:cdata(Username)]
                end),
            ?Xmlel(<<"password">>, [], []),
            ?Xmlel(<<"digest">>, [], []),
            ?Xmlel(<<"resource">>, [], [])
        ])
    ).

%% @doc Make an `<iq>' to notify a successfull authentication.
-spec(success/1 ::
(
  Stanza_IQ_Set::exmpp_stanza:iq_set())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result()
).

success(Stanza_IQ_Set) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:result(Stanza_IQ_Set).

%% @spec (Password_IQ, Condition) -> Failure_IQ
%%     Password_IQ = exxml:xmlel()
%%     Condition = <<"not_authorized">> | <<"conflict">> | <<"not_acceptable">>
%%     Failure_IQ = exxml:xmlel()
%% @doc Make an `<iq>' to notify a successfull authentication.
-spec(failure/2 ::
(
  Stanza_IQ_Set :: exmpp_stanza:iq_set(),
  Error_Condition :: exmpp_server_legacy_auth:error_condition())
    -> Stanza_IQ_Error::exmpp_stanza:iq_error()
).

failure(Stanza_IQ_Set, Error_Condition) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:error_without_original(Stanza_IQ_Set,
        exxml:set_attr(exmpp_stanza:error(Error_Condition),
            <<"code">>,
            case Error_Condition of
                <<"not-authorized">> -> <<"401">>;
                <<"conflict">>       -> <<"409">>;
                <<"not-acceptable">> -> <<"406">>
            end)
    ).

%% --------------------------------------------------------------------
%% Accessing informations.
%% --------------------------------------------------------------------

%% @doc Tell if the initiating entity asks for the authentication fields.
-spec(want_fields/1 ::
(
  Stanza_IQ_Get::exmpp_stanza:iq_get())
    -> Want_Fields::boolean()
).

want_fields(Stanza_IQ_Get) when ?IS_IQ(Stanza_IQ_Get) ->
    case exmpp_iq:get_type(Stanza_IQ_Get) of
        <<"get">> ->
            case exmpp_iq:get_request(Stanza_IQ_Get) of
                #xmlel{name = <<"query">>} -> true;
                _                          -> false
            end;
        _ ->
            false
    end;
want_fields(_Stanza) ->
    false.

%% @doc Extract credentials from the `Password_IQ'.
%%
%% For digest, hexadecimal content is decoded.
-spec(get_credentials/1 ::
(
  Stanza_IQ_Set::exmpp_stanza:iq_set())
    -> Credentials :: {
         Username :: exmpp_client_legacy_auth:username(),
         Password :: {'plain' | 'digest', exmpp_client_legacy_auth:password()},
         Resource :: exmpp_client_legacy_auth:resource()
       }
).

get_credentials(Stanza_IQ_Set) when ?IS_IQ(Stanza_IQ_Set) ->
    case exxml:get_els(exmpp_iq:get_request(Stanza_IQ_Set)) of
        Xmlels when length(Xmlels) == 3 ->
            get_credentials2(Xmlels, {undefined, undefined, undefined});
        _ ->
            throw({legacy_auth, get_credentials, invalid_iq, Stanza_IQ_Set})
    end.

%%
-spec(get_credentials2/2 ::
(
  Xmlels      :: [exmpp_client_legacy_auth:xmlel_username() |
                  exmpp_client_legacy_auth:xmlel_password() |
                  exmpp_client_legacy_auth:xmlel_resource() ,...]
               | [exmpp_client_legacy_auth:xmlel_username() |
                  exmpp_client_legacy_auth:xmlel_digest()   |
                  exmpp_client_legacy_auth:xmlel_resource() ,...],
  Credentials :: {
    Username :: exmpp_client_legacy_auth:username() | undefined,
    Password :: {'plain' | 'digest', exmpp_client_legacy_auth:password()} | undefined,
    Resource :: exmpp_client_legacy_auth:resource() | undefined
  })
    -> Credentials :: {
         Username :: exmpp_client_legacy_auth:username(),
         Password :: {'plain' | 'digest', exmpp_client_legacy_auth:password()},
         Resource :: exmpp_client_legacy_auth:resource()
       }
).

get_credentials2([Xmlel_Username | Xmlels], {_Username, Password, Resource})
  when Xmlel_Username#xmlel.name == <<"username">> ->
    get_credentials2(Xmlels,
        {exxml:get_cdata(Xmlel_Username), Password, Resource});
get_credentials2([Xmlel_Password | Xmlels], {Username, _Password, Resource})
  when Xmlel_Password#xmlel.name == <<"password">> ->
    get_credentials2(Xmlels,
        {Username, {'plain', exxml:get_cdata(Xmlel_Password)}, Resource});
get_credentials2([Xmlel_Digest | Xmlels], {Username, _Password, Resource})
  when Xmlel_Digest#xmlel.name == <<"digest">> ->
    get_credentials2(Xmlels,
        {Username,
         {'digest', list_to_binary(unhex(binary_to_list(exxml:get_cdata(Xmlel_Digest))))},
         Resource});
get_credentials2([Xmlel_Resource | Xmlels], {Username, Password, _Resource})
  when Xmlel_Resource#xmlel.name == <<"resource">> ->
    get_credentials2(Xmlels,
        {Username, Password, exxml:get_cdata(Xmlel_Resource)});
get_credentials2([Xmlel | _Xmlels], _Credentials) ->
    throw({legacy_auth, get_credentials, invalid_field, Xmlel});
get_credentials2([], {undefined, _Password, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'username'});
get_credentials2([], {_Username, undefined, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'password'});
get_credentials2([], {_Username, _Password, undefined}) ->
    throw({legacy_auth, get_credentials, missing_field, 'resource'});
get_credentials2([], Credentials) ->
    Credentials.

%% --------------------------------------------------------------------
%% Tools.
%% --------------------------------------------------------------------

%% @spec (Hex) -> Plain
%%     Hex = string()
%%     Plain = list()
%% @doc Decode hexadecimal string.

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
