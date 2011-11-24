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

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Mechanisms) -> Feature
%%     Mechanisms = [binary() ]
%%     Feature = exml:xmlel()
%% @throws {sasl, feature_announcement, invalid_mechanisms_list, []} |
%%         {sasl, feature_announcement, invalid_mechanism, Mechanism}
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature(Mechanisms) ->
    {xmlel, <<"mechanisms">>, [{<<"xmlns">>, ?NS_SASL}], mechanisms_list(Mechanisms)}.

mechanisms_list([]) ->
    throw({sasl, feature_announcement, invalid_mechanisms_list, []});
mechanisms_list(Mechanisms) ->
	[{xmlel, <<"mechanism">>, [], [{cdata, Mechanism}]} || Mechanism <- Mechanisms].

%% --------------------------------------------------------------------
%% SASL exchange.
%% --------------------------------------------------------------------

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

%% @spec (Challenge) -> Challenge_El
%%     Challenge = binary() | none
%%     Challenge_El = exml:xmlel()
%% @doc Prepare a `<challenge/>' element with the given challenge.
%%
%% `Challenge' will be Base64-encoded by this function.

challenge(none) ->
	{xmlel, <<"challenge">>, [{<<"xmlns">>, ?NS_SASL}], []};
challenge(Challenge) ->
	{xmlel, <<"challenge">>, [{<<"xmlns">>, ?NS_SASL}], [{cdata, base64:encode(Challenge)}]}.

%% @spec () -> Success_El
%%     Success_El = exml:xmlel()
%% @doc Prepare a `<success/>' element.

success() ->
    success(none).

%% @spec (Data) -> Success_El
%%     Data = binary() | none
%%     Success_El = exml:xmlel()
%% @doc Prepare a `<success/>' element with supplied XML character data.
%% `Data' will be Base64-encoded by this function.

success(none) ->
	{xmlel, <<"success">>, [{<<"xmlns">>, ?NS_SASL}], []};
success(Data) ->
	{xmlel, <<"success">>, [{<<"xmlns">>, ?NS_SASL}], [{cdata, base64:encode(Data)}]}.

%% @spec () -> Failure
%%     Failure = exml:xmlel()
%% @doc Prepare a `<failure/>' element.

failure() ->
	{xmlel, <<"failure">>, [{<<"xmlns">>, ?NS_SASL}],[]}.

%% @spec (Condition) -> Failure
%%     Condition = binary()
%%     Failure = exml:xmlel()
%% @doc Prepare a `<failure/>' element with a defined condition.

failure(Condition) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({sasl, failure, invalid_condition, Condition})
    end,
    exml:append_child(failure(), {xmlel,Condition, [], []}).

%% @spec (Condition, Text) -> Failure
%%     Condition = binary()
%%     Text = binary()
%%     Failure = exml:xmlel()
%% @doc Prepare a `<failure/>' element with a defined condition and text.

failure(Condition, <<>>) ->
    failure(Condition);
failure(Condition, Text) ->
	exml:append_child(failure(Condition), {xmlel, <<"text">>, [], [{cdata, Text}]}).

%% @spec (El) -> Type
%%     El = exml:xmlel()
%%     Type = Auth | Response | Abort
%%     Auth = {auth, Mechanism, none | binary()}
%%     Mechanism = binary()
%%     Response = {response, binary()}
%%     Abort = abort
%% @throws {sasl, next_step, unexpected_element, El}
%% @doc Extract the response that the initiating entity sent.
%%
%% Any response data is Base64-decoded.

next_step({xmlel, <<"auth">>, _, _} = El) ->
    Mechanism = exml:get_attribute(El, <<"mechanism">>),
    case exmpp_utils:strip(exml:get_cdata(El)) of
        <<>>      -> {auth, Mechanism, none};
        <<"=">>     -> {auth, Mechanism, <<>>};
        Encoded -> {auth, Mechanism, base64:decode(Encoded)}
    end;
next_step({xmlel, <<"response">>, _, _} = El) ->
    Encoded = exml:get_cdata(El),
    {response, base64:decode(Encoded)};
next_step({xmlel, <<"abort">>, _, _}) ->
    abort;
next_step(El) ->
    throw({sasl, next_step, unexpected_element, El}).
