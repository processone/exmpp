% $Id$

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
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature announcement.
-export([
  feature/1
]).

% SASL exchange.
-export([
  challenge/1,
  success/0,
  failure/0,
  failure/1,
  next_step/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec (Mechanisms) -> Feature
%%     Mechanisms = [string()]
%%     Feature = exmpp_xml:xmlel()
%% @throws {sasl, feature_announcement, invalid_mechanisms_list, []} |
%%         {sasl, feature_announcement, invalid_mechanism, Mechanism}
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature(Mechanisms) ->
    Children = mechanisms_list(Mechanisms),
    #xmlel{
      ns = ?NS_SASL,
      name = 'mechanisms',
      children = Children
    }.

mechanisms_list([]) ->
    throw({sasl, feature_announcement, invalid_mechanisms_list, []});
mechanisms_list(Mechanisms) ->
    mechanisms_list2(Mechanisms, []).

mechanisms_list2([Mechanism | Rest], Children) ->
    case io_lib:deep_char_list(Mechanism) of
        true ->
            Child = #xmlel{
              ns = ?NS_SASL,
              name = 'mechanism'
            },
            mechanisms_list2(Rest,
              [exmpp_xml:set_cdata(Child, Mechanism) | Children]);
        false ->
            throw({sasl, feature_announcement, invalid_mechanism, Mechanism})
    end;
mechanisms_list2([], Children) ->
    lists:reverse(Children).

% --------------------------------------------------------------------
% SASL exchange.
% --------------------------------------------------------------------

standard_conditions() ->
    [
      {'aborted'},
      {'incorrect-encoding'},
      {'invalid-authzid'},
      {'invalid-mechanism'},
      {'mechanism-too-weak'},
      {'not-authorized'},
      {'temporary-auth-failure'}
    ].

%% @spec (Challenge) -> Challenge_El
%%     Challenge = string()
%%     Challenge_El = exmpp_xml:xmlel()
%% @doc Prepare a `<challenge/>' element with the given challenge.
%%
%% `Challenge' will be Base64-encoded by this function.

challenge(Challenge) ->
    El = #xmlel{
      ns = ?NS_SASL,
      name = 'challenge'
    },
    exmpp_xml:set_cdata(El, exmpp_utils:encode_base64(Challenge)).

%% @spec () -> Success_El
%%     Success_El = exmpp_xml:xmlel()
%% @doc Prepare a `<success/>' element.

success() ->
    #xmlel{
      ns = ?NS_SASL,
      name = 'success'
    }.

%% @spec () -> Failure
%%     Failure = exmpp_xml:xmlel()
%% @doc Prepare a `<failure/>' element.

failure() ->
    #xmlel{
      ns = ?NS_SASL,
      name = 'failure'
    }.

%% @spec (Condition) -> Failure
%%     Condition = atom()
%%     Failure = exmpp_xml:xmlel()
%% @doc Prepare a `<failure/>' element.

failure(Condition) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({sasl, failure, invalid_condition, Condition})
    end,
    Condition_El = #xmlel{
      ns = ?NS_SASL,
      name = Condition
    },
    exmpp_xml:append_child(failure(), Condition_El).

%% @spec (El) -> Type
%%     El = exmpp_xml:xmlel()
%%     Type = Auth | Response | Abort
%%     Auth = {auth, Mechanism, none | string()}
%%     Response = {response, string()}
%%     Abort = abort
%% @throws {sasl, next_step, unexpected_element, El}
%% @doc Extract the response that the initiating entity sent.
%%
%% Any response data is Base64-decoded.

next_step(#xmlel{ns = ?NS_SASL, name = 'auth'} = El) ->
    Mechanism = exmpp_xml:get_attribute(El, 'mechanism', undefined),
    case exmpp_utils:strip(exmpp_xml:get_cdata_as_list(El)) of
        ""      -> {auth, Mechanism, none};
        "="     -> {auth, Mechanism, ""};
        Encoded -> {auth, Mechanism, exmpp_utils:decode_base64(Encoded)}
    end;
next_step(#xmlel{ns = ?NS_SASL, name = 'response'} = El) ->
    Encoded = exmpp_xml:get_cdata_as_list(El),
    {response, exmpp_utils:decode_base64(Encoded)};
next_step(#xmlel{ns = ?NS_SASL, name = 'abort'}) ->
    abort;
next_step(El) ->
    throw({sasl, next_step, unexpected_element, El}).
