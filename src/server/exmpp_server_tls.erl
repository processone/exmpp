% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_tls).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  feature/0,
  feature/1,
  proceed/0,
  failure/0
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

feature() ->
    feature(false).

feature(Is_Required) ->
    Feature = #xmlnselement{
      ns = ?NS_TLS,
      name = 'starttls',
      children = []
    },
    if
        Is_Required ->
            Required = #xmlnselement{
              ns = ?NS_TLS,
              name = 'required',
              children = []
            },
            exmpp_xml:append_child(Feature, Required);
        true ->
            Feature
    end.

% --------------------------------------------------------------------
% TLS negotiation.
% --------------------------------------------------------------------

proceed() ->
    #xmlnselement{
      ns = ?NS_TLS,
      name = 'proceed',
      children = []
    }.

failure() ->
    #xmlnselement{
      ns = ?NS_TLS,
      name = 'failure',
      children = []
    }.
