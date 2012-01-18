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
%% side of SASL authentication.
%%
%% <p>
%% Note that it doesn't implement SASL, only feature negotiation at the
%% XMPP level.
%% </p>

-module(exmpp_server_sasl).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    feature/1
]).

%% SASL exchange.
-export([
    challenge/1,
    success/0,
    success/1,
    failure/0,
    failure/1,
    failure/2,
    next_step/1
]).

%%
-export_type([
  mechanism/0,
  mechanisms/0,
  challenge/0,
  additional_data/0
]).

-type(mechanism() :: binary()).
-type(mechanisms() :: [Mechanism::exmpp_server_sasl:mechanism(),...]).
-type(challenge() :: <<>> | binary()).
-type(additional_data() :: binary()).

-export_type([
    error_condition/0,
    standard_condition/0,
    standard_conditions/0
]).

-type(error_condition() :: binary()).
-type(standard_condition() :: {Error_Condition::exmpp_server_sasl:error_condition()}).
-type(standard_conditions()
  :: [Standard_Condtion::exmpp_server_sasl:standard_condition(),...]
).

-export_type([
  xmlel_mechanism/0,
  xmlel_mechanisms/0,
  xmlel_challenge/0,
  xmlel_failure/0,
  xmlel_success/0
]).


-type(xmlel_mechanisms()
  :: #xmlel{
         name     :: <<_:80>>,
         attrs    :: [],
         children :: [Xmlel_Mechanism::exmpp_server_sasl:xmlel_mechanism(),...]
     }
).

-type(xmlel_mechanism()
  :: #xmlel{
         name     :: exmpp_server_sasl:mechanism(),
         attrs    :: [],
         children :: [{'cdata', Mechanism::exmpp_server_sasl:mechanism()},...]
     }
).

-type(xmlel_challenge()
  :: #xmlel{
         name     :: <<_:72>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_SASL::<<_:256>>},...],
         children :: []
                   | [{'cdata', Challenge::exmpp_server_sasl:challenge()},...]
     }
).

-type(xmlel_failure()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_SASL::<<_:256>>},...],
         children :: [
            #xmlel{
                name     :: exmpp_server_sasl:error_condition(),
                attrs    :: [],
                children :: []
                          | [{'cdata', Error_Text::exmpp_stream:error_text()},...]
            }
         ,...]
                     | []
     }
).

-type(xmlel_success()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_SASL::<<_:256>>},...],
         children :: []
                   | [{'cdata', exmpp_server_sasl:additional_data()},...]
     }
).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@SASL(Name, Attrs, Children),
(
    exxml:element(?NS_SASL, Name, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @throws {sasl, feature_announcement, invalid_mechanisms_list, []} |
%%         {sasl, feature_announcement, invalid_mechanism, Mechanism}
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.
-spec(feature/1 ::
(
  Mechanisms::exmpp_server_sasl:mechanisms())
    -> Xmlel_Mechanisms::exmpp_server_sasl:xmlel_mechanisms()
).

feature(Mechanisms) ->
    ?Xmlel@SASL(<<"mechanisms">>, [], mechanisms_list(Mechanisms)).

%%
-spec(mechanisms_list/1 ::
(
  Mechanisms::exmpp_server_sasl:mechanisms())
    -> Xmlels_Mechanism :: [Xmlel_Mechanism::exmpp_server_sasl:xmlel_mechanism(),...]
).

mechanisms_list([]) ->
    throw({sasl, feature_announcement, invalid_mechanisms_list, []});
mechanisms_list(Mechanisms) ->
    [?Xmlel(<<"mechanism">>, [], [exxml:cdata(Mechanism)]) || Mechanism <- Mechanisms].

%% --------------------------------------------------------------------
%% SASL exchange.
%% --------------------------------------------------------------------

-spec(standard_conditions/0
  :: () -> Standard_Conditions::exmpp_server_sasl:standard_conditions()
).

standard_conditions() ->
    [
     {<<"aborted">>},
     {<<"incorrect-encoding">>},
     {<<"invalid-authzid">>},
     {<<"invalid-mechanism">>},
     {<<"mechanism-too-weak">>},
     {<<"not-authorized">>},
     {<<"temporary-auth-failure">>},
     %% rfc3920bis
     {<<"account-disabled">>},
     {<<"credentials-expired">>},
     {<<"encryption-required">>},
     {<<"malformed-request">>},
     {<<"undefined-condition">>}
    ].

%% @doc Prepare a `<challenge/>' element with the given challenge.
%%
%% `Challenge' will be Base64-encoded by this function.
-spec(challenge/1 ::
(
  Challenge :: exmpp_server_sasl:challenge() | none)
    -> Xmlel_Challenge::exmpp_server_sasl:xmlel_challenge()
).

challenge(none) ->
    ?Xmlel@SASL(<<"challenge">>, [], []);
challenge(Challenge) ->
    ?Xmlel@SASL(<<"challenge">>, [], [exxml:cdata(base64:encode(Challenge))]).

%% @doc Prepare a `<success/>' element.
-spec(success/0 :: () -> Xmlel_Sucess::exmpp_server_sasl:xmlel_success()).

success() ->
    success(none).

%% @doc Prepare a `<success/>' element with supplied XML character data.
%% `Data' will be Base64-encoded by this function.
-spec(success/1 ::
(
  Additional_Data :: exmpp_server_sasl:additional_data() | none)
    -> Xmlel_Sucess::exmpp_server_sasl:xmlel_success()
).

success(none) ->
    ?Xmlel@SASL(<<"success">>, [], []);
success(Additional_Data) ->
    ?Xmlel@SASL(<<"success">>, [], [exxml:cdata(base64:encode(Additional_Data))]).

%% @doc Prepare a `<failure/>' element.
-spec(failure/0 :: () -> Xmlel_Failure::exmpp_server_sasl:xmlel_failure()).

failure() ->
    ?Xmlel@SASL(<<"failure">>, [], []).

%% @doc Prepare a `<failure/>' element with a defined condition.
-spec(failure/1 ::
(
  Error_Condition::exmpp_server_sasl:error_condition())
    -> Xmlel_Failure::exmpp_server_sasl:xmlel_failure()
).

failure(Error_Condition) ->
    case lists:keymember(Error_Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({sasl, failure, invalid_condition, Error_Condition})
    end,
    exxml:append_child(failure(), ?Xmlel(Error_Condition, [], [])).

%% @doc Prepare a `<failure/>' element with a defined condition and text.
-spec(failure/2 ::
(
  Error_Condition :: exmpp_server_sasl:error_condition(),
  Error_Text      :: <<>> | exmpp_stream:error_text())
    -> Xmlel_Failure::exmpp_server_sasl:xmlel_failure()
).

failure(Error_Condition, <<>> = _Error_Text) ->
    failure(Error_Condition);
failure(Error_Condition, Error_Text) ->
    exxml:append_child(failure(Error_Condition),
        ?Xmlel(<<"text">>, [], [exxml:cdata(Error_Text)])).

%% @throws {sasl, next_step, unexpected_element, El}
%% @doc Extract the response that the initiating entity sent.
%%
%% Any response data is Base64-decoded.
-spec(next_step/1 ::
(
  Xmlel_Auth::exmpp_client_sasl:xmlel_auth())
    -> {'auth',
        Mechanism        :: exmpp_server_sasl:mechanism() | undefined,
        Initial_Response :: exmpp_client_sasl:initial_response() | none} ;
(
  Xmlel_Response::exmpp_client_sasl:xmlel_response())
    -> {'response',
        Challenge::exmpp_client_sasl:challenge()} ;
(
  Xmlel_Abort::exmpp_client_sasl:xmlel_abort())
    -> 'abort'
).

next_step(Xmlel_Auth) when Xmlel_Auth#xmlel.name == <<"auth">> ->
    {'auth',
     exxml:get_attribute(Xmlel_Auth, <<"mechanism">>),
     case exmpp_utils:strip(exxml:get_cdata(Xmlel_Auth)) of
         <<>>    -> none;
         <<"=">> -> <<>>;
         CData   -> base64:decode(CData)
     end};
next_step(Xmlel_Response) when Xmlel_Response#xmlel.name == <<"response">> ->
    {'response', base64:decode(exxml:get_cdata(Xmlel_Response))};
next_step(#xmlel{name = <<"abort">>}) ->
    'abort';
next_step(Xmlel) ->
    throw({sasl, next_step, unexpected_element, Xmlel}).
