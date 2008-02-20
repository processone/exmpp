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
-export([connect/3, send/2, close/2]).

%% Internal export
-export([receiver/3]).

%% Connect to XMPP server
%% Returns:
%% {ok, Ref} | {error, Reason}
%% Ref is a socket
connect(ClientPid, StreamRef, {Host, Port}) ->
    case gen_tcp:connect(Host, Port, [{packet,0},
				      binary,
				      {active, once},
				      {reuseaddr, true}], 30000) of
	{ok, Socket} ->
	    %% TODO: Hide receiver failures in API
	    ReceiverPid = spawn_link(?MODULE, receiver,
				     [ClientPid, Socket, StreamRef]),
	    gen_tcp:controlling_process(Socket, ReceiverPid),
	    {Socket, ReceiverPid};
	{error, Reason} ->
	    erlang:throw({socket_error, Reason})
    end.

close(Socket, ReceiverPid) ->
    ReceiverPid ! stop,
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
receiver(ClientPid, Socket, StreamRef) ->
    receiver_loop(ClientPid, Socket, StreamRef).
    
receiver_loop(ClientPid, Socket, StreamRef) ->
    receive
	stop ->
	    ok;
	{tcp, Socket, Data} ->
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Data),
	    inet:setopts(Socket, [{active, once}]),
	    receiver_loop(ClientPid, Socket, NewStreamRef);
	{tcp_closed, Socket} ->
	    gen_fsm:sync_send_all_state_event(ClientPid, tcp_closed)
    end.
