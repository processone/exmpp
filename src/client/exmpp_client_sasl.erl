% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_sasl).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  announced_mechanisms/1,
  selected_mechanism/1,
  selected_mechanism/2,
  response/1,
  abort/0,
  next_step/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

announced_mechanisms(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_SASL, 'mechanisms') of
        false      -> [];
        Mechanisms -> announced_mechanisms(Mechanisms)
    end;
announced_mechanisms(#xmlnselement{ns = ?NS_SASL, name = 'mechanisms',
  children = Children}) ->
    announced_mechanisms2(Children, []).

announced_mechanisms2(
  [#xmlnselement{ns = ?NS_SASL, name = 'mechanism'} = El | Rest], Result) ->
    Mechanism = exmpp_xml:get_cdata(El),
    announced_mechanisms2(Rest, Result ++ [Mechanism]);
announced_mechanisms2([], Result) ->
    Result.

% --------------------------------------------------------------------
% SASL exchange.
% --------------------------------------------------------------------

selected_mechanism(Mechanism) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = 'auth',
      children = []
    },
    exmpp_xml:set_attribute(El, 'mechanism', Mechanism).

selected_mechanism(Mechanism, "") ->
    El = selected_mechanism(Mechanism),
    exmpp_xml:set_cdata(El, "=");
selected_mechanism(Mechanism, Initial_Response) ->
    El = selected_mechanism(Mechanism),
    exmpp_xml:set_cdata(El, exmpp_internals:encode_base64(Initial_Response)).

response(Response) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = 'response',
      children = []
    },
    exmpp_xml:set_cdata(El, exmpp_internals:encode_base64(Response)).

abort() ->
    #xmlnselement{
      ns = ?NS_SASL,
      name = 'abort',
      children = []
    }.

next_step(#xmlnselement{ns = ?NS_SASL, name = 'challenge'} = El) ->
    Encoded = exmpp_xml:get_cdata(El),
    {challenge, exmpp_internals:decode_base64(Encoded)};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'failure',
  children = [#xmlnselement{ns = ?NS_SASL, name = Reason}]}) ->
    {failure, Reason};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'failure'}) ->
    {failure, undefined};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'success'} = El) ->
    Encoded = exmpp_xml:get_cdata(El),
    {success, exmpp_internals:decode_base64(Encoded)}.
