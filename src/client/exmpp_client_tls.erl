% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_tls).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  announced_support/1,
  starttls/0
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

announced_support(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_TLS, 'starttls') of
        false   -> none;
        Methods -> announced_support(Methods)
    end;
announced_support(#xmlnselement{ns = ?NS_TLS, name = 'starttls',
  children = []}) ->
    optional;
announced_support(#xmlnselement{ns = ?NS_TLS, name = 'starttls',
  children = [#xmlnselement{ns = ?NS_TLS, name = 'required'}]}) ->
    required;
announced_support(#xmlnselement{ns = ?NS_TLS, name = 'starttls'}) ->
    invalid.

% --------------------------------------------------------------------
% TLS negotiation.
% --------------------------------------------------------------------

starttls() ->
    #xmlnselement{
      ns = ?NS_TLS,
      name = 'starttls',
      children = []
    }.
