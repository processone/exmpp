% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_sasl).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  feature/1,
  challenge/1,
  failure/0,
  failure/1,
  next_step/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

feature(Mechanisms) ->
    case mechanisms_list(Mechanisms) of
        {error, Reason} ->
            {error, Reason};
        Children ->
            #xmlnselement{
              ns = ?NS_SASL,
              name = 'mechanisms',
              children = Children
            }
    end.

mechanisms_list(Mechanisms) ->
    mechanisms_list2(Mechanisms, []).

mechanisms_list2([Mechanism | Rest], Children) ->
    case io_lib:deep_char_list(Mechanism) of
        true ->
            Child = #xmlnselement{
              ns = ?NS_SASL,
              name = 'mechanism',
              children = []
            },
            mechanisms_list2(Rest,
              Children ++ [exmpp_xml:set_cdata(Child, Mechanism)]);
        false ->
            {error, bad_mechanisms_list}
    end;
mechanisms_list2([], Children) ->
    Children.

% --------------------------------------------------------------------
% SASL exchange.
% --------------------------------------------------------------------

challenge(Challenge) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = 'challenge',
      children = []
    },
    exmpp_xml:set_cdata(El, exmpp_internals:encode_base64(Challenge)).

failure() ->
    #xmlnselement{
      ns = ?NS_SASL,
      name = 'failure',
      children = []
    }.

failure(Reason) ->
    El = #xmlnselement{
      ns = ?NS_SASL,
      name = list_to_atom(Reason),
      children = []
    },
    exmpp_xml:append_child(failure(), El).

next_step(#xmlnselement{ns = ?NS_SASL, name = 'auth'} = El) ->
    Mechanism = exmpp_xml:get_attribute(El, 'mechanism'),
    case string:strip(exmpp_xml:get_cdata(El)) of
        ""      -> {auth, Mechanism};
        "="     -> {auth, Mechanism, ""};
        Encoded -> {auth, Mechanism, exmpp_internals:decode_base64(Encoded)}
    end;
next_step(#xmlnselement{ns = ?NS_SASL, name = 'response'} = El) ->
    Encoded = exmpp_xml:get_cdata(El),
    {response, exmpp_internals:decode_base64(Encoded)};
next_step(#xmlnselement{ns = ?NS_SASL, name = 'abort'}) ->
    abort;
next_step(#xmlnselement{}) ->
    {error, unexpected_stanza}.
