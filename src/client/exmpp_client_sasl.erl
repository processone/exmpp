% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% The module <strong>{@module}</strong> implements the initiating
%% entity side of SASL authentication.
%%
%% <p>
%% Note that it doesn't implement SASL, only feature negotiation at the
%% XMPP level.
%% </p>

-module(exmpp_client_sasl).
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature announcement.
-export([
  announced_mechanisms/1
]).

% SASL exchange.
-export([
  selected_mechanism/1,
  selected_mechanism/2,
  response/1,
  abort/0,
  next_step/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec (Features_Annoucenement) -> Mechanisms
%%     Features_Announcement = exmpp_xml:xmlnselement()
%%     Mechanisms = [string()]
%% @throws {sasl, announced_mechanisms, invalid_feature, Feature} |
%%         {sasl, announced_mechanisms, invalid_mechanism, El}
%% @doc Return the list of SASL mechanisms announced by the receiving entity.

announced_mechanisms(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_SASL, 'mechanisms') of
        undefined  -> [];
        Mechanisms -> announced_mechanisms2(Mechanisms)
    end.

announced_mechanisms2(#xmlnselement{children = []} = Feature) ->
    throw({sasl, announced_mechanisms, invalid_feature, Feature});
announced_mechanisms2(#xmlnselement{children = Children}) ->
    announced_mechanisms3(Children, []).

announced_mechanisms3(
  [#xmlnselement{ns = ?NS_SASL, name = 'mechanism'} = El | Rest], Result) ->
    case exmpp_xml:get_cdata_as_list(El) of
        "" ->
            throw({sasl, announced_mechanisms, invalid_mechanism, El});
        Mechanism ->
            announced_mechanisms3(Rest, [Mechanism | Result])
    end;
announced_mechanisms3([El | _Rest], _Result) ->
    throw({sasl, announced_mechanisms, invalid_mechanism, El});
announced_mechanisms3([], Result) ->
    lists:reverse(Result).

% --------------------------------------------------------------------
% SASL exchange.
% --------------------------------------------------------------------

%% @spec (Mechanism) -> Auth
%%     Mechanism = string()
%%     Auth = exmpp_xml:xmlnselement()
%% @doc Prepare an `<auth/>' element with the selected mechanism.

selected_mechanism(Mechanism) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = 'auth',
      children = []
    },
    exmpp_xml:set_attribute(El, 'mechanism', Mechanism).

%% @spec (Mechanism, Initial_Response) -> Auth
%%     Mechanism = string()
%%     Initial_Response = string()
%%     Auth = exmpp_xml:xmlnselement()
%% @doc Prepare an `<auth/>' element with the selected mechanism.
%%
%% The initial response will be Base64-encoded before inclusion.

selected_mechanism(Mechanism, "") ->
    El = selected_mechanism(Mechanism),
    exmpp_xml:set_cdata(El, "=");
selected_mechanism(Mechanism, Initial_Response) ->
    El = selected_mechanism(Mechanism),
    exmpp_xml:set_cdata(El, exmpp_internals:encode_base64(Initial_Response)).

%% @spec (Response_Data) -> Response
%%     Response_Data = string()
%%     Response = exmpp_xml:xmlnselement()
%% @doc Prepare a `<response/>' element to send the challenge's response.
%%
%% `Response_Data' will be Base64-encoded.

response(Response_Data) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = 'response',
      children = []
    },
    exmpp_xml:set_cdata(El, exmpp_internals:encode_base64(Response_Data)).

%% @spec () -> Abort
%%     Abort = exmpp_xml:xmlnselement()
%% @doc Make a `<abort/>' element.

abort() ->
    #xmlnselement{
      ns = ?NS_SASL,
      name = 'abort',
      children = []
    }.

%% @spec (El) -> Type
%%     El = exmpp_xml:xmlnselement() 
%%     Type = Challenge | Success | Failure
%%     Challenge = {challenge, string()}
%%     Success = {success, string()}
%%     Failure = {failure, Condition | undefined}
%%     Condition = atom()
%% @doc Extract the challenge or the ending element that the receiving
%% entity sent.
%%
%% Any challenge or success data is Base64-decoded.

next_step(#xmlnselement{ns = ?NS_SASL, name = 'challenge'} = El) ->
    Encoded = exmpp_xml:get_cdata_as_list(El),
    {challenge, exmpp_internals:decode_base64(Encoded)};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'failure',
  children = [#xmlnselement{ns = ?NS_SASL, name = Condition}]}) ->
    {failure, Condition};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'failure'}) ->
    {failure, undefined};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'success'} = El) ->
    Encoded = exmpp_xml:get_cdata_as_list(El),
    {success, exmpp_internals:decode_base64(Encoded)}.
