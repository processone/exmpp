% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> manages simple TCP/IP socket
%% connections to an XMPP server.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

-module(exmpp_tcp).

%% Behaviour exmpp_gen_transport ?
-export([connect/2, send/2, close/1]).

%% Internal export
-export([receiver/2]).

%% Connect to XMPP server
%% Returns:
%% {ok, Ref} | {error, Reason}
%% Ref is a socket
connect(StreamRef, {Host, Port}) ->
    case gen_tcp:connect(Host, Port, [{packet,0},
				      binary,
				      {active, false},
				      {reuseaddr, true}], 30000) of
	{ok, Socket} ->
	    %% TODO: Hide receiver failures in API
	    _ReceiverPid = spawn_link(?MODULE, receiver, [Socket, StreamRef]),
	    Socket;
	{error, Reason} ->
	    erlang:throw({socket_error, Reason})
    end.

close(Socket) ->
    %% TODO: Close receiver
    gen_tcp:close(Socket).

send(Socket, XMLPacket) ->
    %% TODO: document_to_binary to reduce memory consumption
    String = exmpp_xml:document_to_list(XMLPacket),
    case gen_tcp:send(Socket, String) of
	ok -> ok;
	_Other -> {error, send_failed}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
receiver(Socket, StreamRef) ->
    process_flag(trap_exit, true),    
    receiver_loop(Socket, StreamRef).
    
receiver_loop(Socket, StreamRef) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Data),
	    receiver_loop(Socket, NewStreamRef);
	{'EXIT', _} ->
	    ok;
	{error, _Reason} -> 
	    ok
    end.
