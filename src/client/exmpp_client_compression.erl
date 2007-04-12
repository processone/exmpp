% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_compression).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  announced_methods/1,
  selected_method/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

announced_methods(#xmlnselement{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element_by_name(El, ?NS_COMPRESS, 'compression') of
        false   -> [];
        Methods -> announced_methods(Methods)
    end;
announced_methods(#xmlnselement{ns = ?NS_COMPRESS, name = 'compression',
  children = Children}) ->
    announced_methods2(Children, []).

announced_methods2(
  [#xmlnselement{ns = ?NS_COMPRESS, name = 'method'} = El | Rest], Result) ->
    Method = exmpp_xml:get_cdata(El),
    announced_methods2(Rest, Result ++ [Method]);
announced_methods2([], Result) ->
    Result.

% --------------------------------------------------------------------
% Compression negotiation.
% --------------------------------------------------------------------

selected_method(Method) ->
    El = #xmlnselement{
      ns = ?NS_COMPRESS,
      name = 'method',
      children = []
    },
    #xmlnselement{
      ns = ?NS_COMPRESS,
      name = 'compress',
      children = [exmpp_xml:set_cdata(El, Method)]
    }.
