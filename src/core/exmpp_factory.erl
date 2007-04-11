% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides utilities to prepare
%% common XMPP stanzas.

-module(exmpp_factory).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
	features_announcement/1,
	tls_support_announce/1,
	compress_support_announce/1
]).

% --------------------------------------------------------------------
% Features anouncement.
% --------------------------------------------------------------------

features_announcement(Features) ->
	#xmlnselement{
		ns = ?NS_XMPP,
		name = 'features',
		children = Features
	}.

tls_support_announce(Required) ->
	Announce = #xmlnselement{
		ns = ?NS_TLS,
		name = 'starttls',
		children = []
	},
	case Required of
		true ->
			Child = #xmlnselement{
				ns = ?NS_TLS,
				name = 'required',
				children = []
			},
			exmpp_xml:append_child(Announce, Child);
		_ ->
			Announce
	end.

compress_support_announce(Method) ->
	Method_El = #xmlnselement{
		ns = ?NS_COMPRESS,
		name = 'method',
		children = [#xmlcdata{cdata = Method}]
	},
	#xmlnselement{
		ns = ?NS_COMPRESS,
		name = 'compression',
		children = [Method_El]
	}.
