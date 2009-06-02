%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of privacy lists management.

-module(exmpp_server_privacy).
-vsn('$Revision$').

-include("exmpp.hrl").

%% Creating stanza.
-export([
	 list_push/2
	]).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (To, List_Name) -> Push_IQ
%%     To = exmpp_jid:jid() | string()
%%     List_Name = string()
%%     Push_IQ = exmpp_xml:xmlel()
%% @doc Create an `<iq/>' to notify `To' that the privacy list
%% `List_Name' has been created or has changed.

list_push(To, List_Name) ->
    List0 = #xmlel{ns = ?NS_PRIVACY,
		   name = 'list'
		  },
    List = exmpp_xml:set_attribute(List0, 'name', List_Name),
    Query = #xmlel{ns = ?NS_PRIVACY,
		   name = 'query',
		   children = [List]
		  },
    IQ = exmpp_iq:set(?NS_JABBER_CLIENT, Query,
		      exmpp_utils:random_id("push")),
    exmpp_stanza:set_recipient(IQ, To).
