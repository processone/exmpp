%%% File    : xmpp.hrl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Description : 
%%% Created : 17 Oct 2004 by Mickael Remond <mremond@process-one.net>
%%% This code is copyright Process-one (http://www.process-one.net/)

-define(STREAM_CLIENT_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream to='~s' "
	"xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams'>").

-define(STREAM_TRAILER, "</stream:stream>").

-define(defaultserver, "localhost").
-define(defaultport,   5222).

-define(calltimeout, 10000).

%% Logging feature
-define(ERROR_MSG(Format, Args),
	error_logger:error_msg("E(~p:~p:~p): "++Format++"~n",
			       [self(),?MODULE,?LINE]++Args)).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("I(~p:~p:~p): "++Format++"~n",
			      [self(),?MODULE,?LINE]++Args)).

%% Internal state record definition
%% TODO: Rename the record xmpp
-record(state, {socket,
		xml_stream, 
		from_pid, %% This is used in gen_fsm sync calls
		iq_ref_list = [], %% This is used to store tuples of the from {I◊QIdRef, CallerPid} to synchronously wait for IQ results
		callback_module = exmpp_callbacks, %% Default callback module
		username,
		authentication, %% can be {password, Password} or {digest, Digest}
		resource="Jabberlang",
		host="localhost",
		port=5222,
                domain="localhost",
		priority="0",
		show="normal",
		status="",
		%% jabber:iq:version (Os version is read from the system)
		client_name="Jabberlang",
		client_version="0.3",
		%% XMPP client behaviour:
		auto_registration=true
	       }).
