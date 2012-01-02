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

%% The module <strong>{@module}</strong> implements the initiating
%% entity side of SASL authentication.
%%
%% <p>
%% Note that it doesn't implement SASL, only feature negotiation at the
%% XMPP level.
%% </p>

-module(exmpp_client_sasl).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 announced_mechanisms/1
	]).

%% SASL exchange.
-export([
	 selected_mechanism/1,
	 selected_mechanism/2,
	 response/1,
	 abort/0,
	 next_step/1
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> Mechanisms
%%     Features_Announcement = exxml:xmlel()
%%     Mechanisms = [binary()]
%% @throws {sasl, announced_mechanisms, invalid_feature, Feature} |
%%         {sasl, announced_mechanisms, invalid_mechanism, El}
%% @doc Return the list of SASL mechanisms announced by the receiving entity.

announced_mechanisms({xmlel, F, _Attrs, _Children} = El) 
	when F == <<"features">> orelse F == <<"stream:features">> ->
    case exxml:get_element(El, <<"mechanisms">>) of
        undefined  -> [];
        Mechanisms -> announced_mechanisms2(Mechanisms)
    end.

announced_mechanisms2({xmlel, _N, _Attr, []} = Feature) ->
    throw({sasl, announced_mechanisms, invalid_feature, Feature});
announced_mechanisms2(M) ->
    announced_mechanisms3(exxml:get_elements(M), []).

announced_mechanisms3(
  [{xmlel,<<"mechanism">>, _Attrs, _Children} = El | Rest], Result) ->
    case exxml:get_cdata(El) of
        <<>> ->
            throw({sasl, announced_mechanisms, invalid_mechanism, El});
        Mechanism ->
            announced_mechanisms3(Rest, [Mechanism | Result])
    end;
announced_mechanisms3([El | _Rest], _Result) ->
    throw({sasl, announced_mechanisms, invalid_mechanism, El});
announced_mechanisms3([], Result) ->
    lists:reverse(Result).

%% --------------------------------------------------------------------
%% SASL exchange.
%% --------------------------------------------------------------------

%% @spec (Mechanism) -> Auth
%%     Mechanism = binary()
%%     Auth = exxml:xmlel()
%% @doc Prepare an `<auth/>' element with the selected mechanism.

selected_mechanism(Mechanism) ->
	{xmlel, <<"auth">>, [{<<"xmlns">>, ?NS_SASL},{<<"mechanism">>, Mechanism}], []}.

%% @spec (Mechanism, Initial_Response) -> Auth
%%     Mechanism = binary()
%%     Initial_Response = binary()
%%     Auth = exxml:xmlel()
%% @doc Prepare an `<auth/>' element with the selected mechanism.
%%
%% The initial response will be Base64-encoded before inclusion.

selected_mechanism(Mechanism, <<>>) ->
    El = selected_mechanism(Mechanism),
    exxml:append_child(El, {cdata, <<"=">>});
selected_mechanism(Mechanism, Initial_Response) ->
    El = selected_mechanism(Mechanism),
    exxml:append_child(El, {cdata, base64:encode(Initial_Response)}).

%% @spec (Response_Data) -> Response
%%     Response_Data = binary()
%%     Response = exxml:xmlel()
%% @doc Prepare a `<response/>' element to send the challenge's response.
%%
%% `Response_Data' will be Base64-encoded.

response(Response_Data) ->
    {xmlel, <<"response">>, [{<<"xmlns">>, ?NS_SASL}], 
	    [{cdata, base64:encode(Response_Data)}]}.

%% @spec () -> Abort
%%     Abort = exxml:xmlel()
%% @doc Make a `<abort/>' element.

abort() ->
	{xmlel, <<"abort">>, [{<<"xmlns">>, ?NS_SASL}], []}.

%% @spec (El) -> Type
%%     El = exxml:xmlel()
%%     Type = Challenge | Success | Failure
%%     Challenge = {challenge, binary()}
%%     Success = {success, binary()}
%%     Failure = {failure, Condition | undefined}
%%     Condition = binary()
%% @doc Extract the challenge or the ending element that the receiving
%% entity sent.
%%
%% Any challenge or success data is Base64-decoded.

next_step({xmlel, <<"challenge">>, _Attrs, _Children} = El) ->
	{challenge, base64:decode(exxml:get_cdata(El))};
next_step({xmlel, <<"failure">>, _Attrs, _Children} = El) ->
	case exxml:get_elements(El) of
		[{xmlel, Condition, _, _}] ->
			{failure, Condition};
		_ ->
			{failure, undefined}
	end;
next_step({xmlel, <<"success">>, _Attrs, _Children} = El) ->
    {success, base64:decode(exxml:get_cdata(El))}.
