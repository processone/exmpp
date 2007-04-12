% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_server_compression).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  feature/1
]).

% --------------------------------------------------------------------
% Feature announcement.
% --------------------------------------------------------------------

feature(Methods) ->
    % XEP-0138.
    case methods_list(Methods) of
        {error, Reason} ->
            {error, Reason};
        Children ->
            #xmlnselement{
              ns = ?NS_COMPRESS,
              name = 'compression',
              children = Children
            }
    end.

methods_list(Methods) ->
    methods_list2(Methods, []).

methods_list2([Method | Rest], Children) ->
    Child = #xmlnselement{
      ns = ?NS_COMPRESS,
      name = 'method',
      children = []
    },
    methods_list2(Rest,
      Children ++ [exmpp_xml:set_cdata(Child, Method)]);
methods_list2([], Children) ->
    Children.
