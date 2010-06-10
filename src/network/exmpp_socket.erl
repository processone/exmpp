%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Emilio Bustos <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> manages TCP/IP socket
%% connections to an XMPP server with or without TLS/SSL encryption.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>

-module(exmpp_socket).

-export([connect/3, send/2, close/1, close/2, reset_parser/1,
        compress/1
    ]).

%% Internal export
-export([receiver/3]).

reset_parser(ReceiverPid) when is_pid(ReceiverPid) ->
    ReceiverPid ! reset_parser.
    
%% Connect to XMPP server
%% Returns:
%% Ref or throw error
%% Ref is a socket
connect(ClientPid, StreamRef, {Host, Port, Options}) ->
    LocalIP = proplists:get_value(local_ip, Options, undefined),                     
    LocalPort= proplists:get_value(local_port, Options, undefined),                  
    SckType = proplists:get_value(socket_type, Options, gen_tcp),                  
    IPOptions = case LocalIP of                                                                                          
                        undefined -> [];                                           
                        _ ->  case LocalPort of                                                                        
                                undefined -> [{ip, LocalIP}];                     
                                _ -> [{ip, LocalIP}, {port, LocalPort()}]         
                              end                                                                                      
                end,                                                                                                   
    DefaultOptions = [{packet,0}, binary, {active, false}] ++ IPOptions,
    Opts = [{reuseaddr,true}|DefaultOptions],
    case SckType:connect(Host, Port, Opts, 30000) of
	{ok, Socket} ->
            ESocket = {?MODULE, Socket},
	    %% TODO: Hide receiver failures in API
	    ReceiverPid = spawn_link(?MODULE, receiver,
				     [ClientPid, ESocket, StreamRef]),
	    SckType:controlling_process(Socket, ReceiverPid),
            {ESocket, ReceiverPid};
	{error, Reason} ->
	    erlang:throw({socket_error, Reason})
    end.

% we do a synchronous shutdown of the receiver process, 
% because we want to make sure it isn't going be pushing
% more data to the exmpp_xmlstream after closed. This
% avoid the -useless- crash reports produced when the 
% receiver process read data from socket before received
% the stop message, but after the xmlstream was closed.
% See shutdown order in exmpp_session:terminate/3.
close(Socket) ->
    SckType = get_socket_type(Socket),
    SckType:close(Socket).
close(_Socket, ReceiverPid) ->
    ReceiverPid ! stop.

send({_Mod, _Sock} = Socket, XMLPacket) ->
    %% TODO: document_to_binary to reduce memory consumption
    String = exmpp_xml:document_to_list(XMLPacket),
    NewStr = send_data(Socket, String),
 %     case exmpp_internals:gen_send(Socket, String) of
    SckType = get_socket_type(Socket),
    case SckType:send(get_socket(Socket), NewStr) of
	ok -> ok;
	{error, Reason} -> {error, Reason}
    end;
send(Socket, XMLPacket) ->
    String = exmpp_xml:document_to_list(XMLPacket),
    SckType = get_socket_type(Socket),
    case SckType:send(Socket, String) of
	ok -> ok;
	{error, Reason} -> {error, Reason}
    end.

compress(ReceiverPid) ->
    Ref = erlang:make_ref(),
    ReceiverPid ! {compress, self(), Ref},
    receive
        {ok, Ref, Socket} -> {ok, Socket}
    after
        1000 -> timeout
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
receiver(ClientPid, Socket, StreamRef) ->
    receiver_loop(ClientPid, Socket, StreamRef).

receiver_loop(ClientPid, ESocket, StreamRef) ->
    Socket = get_socket(ESocket),
    setopts(ESocket, [{active, once}]),
    receive
	stop -> 
            exmpp_internals:gen_close(ESocket),
	    ok;
	{compress, From, Ref} -> 
            ZSocket = {exmpp_compress, exmpp_compress:enable_compression(ESocket, [])},
            From ! {ok, Ref, ZSocket},
	    receiver_loop(ClientPid, ZSocket, StreamRef);
        {tcp, Socket, Data} ->
            Str = recv_data(ESocket, Data),
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Str),
	    receiver_loop(ClientPid, ESocket, NewStreamRef);
	{ssl, Socket, Data} ->
            Str = recv_data(ESocket, Data),
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Str),
	    receiver_loop(ClientPid, ESocket, NewStreamRef);
	{tcp_closed, Socket} ->
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
	{ssl_closed, Socket} ->
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
	{ssl_error,Socket,Reason} ->
	    error_logger:warning_msg([ssl_error,{ssl_socket,Socket},Reason]),
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
        reset_parser ->
            receiver_loop(ClientPid, ESocket, exmpp_xmlstream:reset(StreamRef))
    end.

get_socket(Socket) when is_port(Socket) ->
    Socket;
get_socket({sslsocket, _, _} = Socket) ->
    Socket;
get_socket({_, Socket, _, _}) ->
    get_socket(Socket);
get_socket({_Type, Socket}) ->
    get_socket(Socket).

get_socket_type({_, Socket}) ->
    get_socket_type(get_socket(Socket));
get_socket_type({sslsocket, _, _}) ->
    ssl;
get_socket_type(Socket) when is_port(Socket)->
    gen_tcp.

recv_data({?MODULE, _Socket}, Data) ->
    Data;
%recv_data({Module, Socket}, ZData) ->
recv_data({exmpp_compress, Socket}, ZData) ->
    {ok, Data} = exmpp_compress:decompress(Socket, ZData),
 %     {ok, Data} = Module:recv_data(Socket, ZData),
    Data.

 % setopts({gen_tcp, Socket}, Opts) ->
 %     inet:setopts(Socket, Opts);
setopts(Socket, Opts) when is_port(Socket) ->
    inet:setopts(Socket, Opts);
setopts({sslsocket, _, _} = Socket, Opts) ->
    ssl:setopts(Socket, Opts);
setopts({_, Socket}, Opts) ->
    setopts(Socket, Opts);
setopts({_, Socket, _, _}, Opts) ->
    setopts(Socket, Opts).

send_data({gen_tcp, _Socket}, Data) ->
    Data;
send_data({?MODULE, _Socket}, Data) ->
    Data;
send_data({_, Socket, _, _}, Data) ->
    send_data(Socket, Data);
 % send_data({Module, Socket}, ZData) ->
send_data({exmpp_compress, Socket}, ZData) ->
    {ok, Data} = exmpp_compress:compress(Socket, ZData),
 %     {ok, Data} = Module:send_data(Socket, ZData),
    send_data(Socket, Data).

