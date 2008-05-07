% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of the TLS feature.
%%
%% <p>
%% Note that it doesn't implement encryption, only feature negotiation
%% at the XMPP level.
%% </p>

-module(exmpp_client_tls).
-vsn('$Revision$').

-include("exmpp.hrl").

% Feature announcement.
-export([
  announced_support/1
]).

% TLS negotiation.
-export([
  starttls/0
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> Support
%%     Features_Announcement = exmpp_xml:xmlnselement()
%%     Support = none | optional | required
%% @throws {tls, announced_support, invalid_announcement, El}
%% @doc Return the kind of TLS negotiation the receiving entity asks for.

announced_support(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_TLS, 'starttls') of
        undefined -> none;
        Child     -> announced_support2(Child)
    end.

announced_support2(#xmlnselement{ns = ?NS_TLS, name = 'starttls',
  children = []}) ->
    optional;
announced_support2(#xmlnselement{ns = ?NS_TLS, name = 'starttls',
  children = [#xmlnselement{ns = ?NS_TLS, name = 'required'}]}) ->
    required;
announced_support2(#xmlnselement{ns = ?NS_TLS, name = 'starttls'} = El) ->
    throw({tls, announced_support, invalid_announcement, El}).

% --------------------------------------------------------------------
% TLS negotiation.
% --------------------------------------------------------------------

%% @spec () -> STARTTLS
%%     STARTTLS = exmpp_xml:xmlnselement()
%% @doc Make an XML element to tell the receiving entity that we want to
%% use TLS.

starttls() ->
    #xmlnselement{
      ns = ?NS_TLS,
      name = 'starttls',
      children = []
    }.
