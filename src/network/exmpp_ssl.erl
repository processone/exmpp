% $Id$

%% @author Will Glozer <will@glozer.net>

%% @doc
%% The module <strong>{@module}</strong> manages SSL socket
%% connections to an XMPP server.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>

-module(exmpp_ssl).

-behavior(gen_server).

-export([connect/3, send/2, close/2]).

-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {socket, stream_ref, client_pid,set_opts_module}).

%% -- client interface --

%% Connect to XMPP server
%% Returns:
%% Ref | {error, Reason}
%% Ref is a pid.
connect(ClientPid, StreamRef, {Host, Port}) ->
    case gen_server:start(?MODULE, [ClientPid, StreamRef, Host, Port], []) of
        {ok, Ref} ->
        	true = link(Ref), 
        	%if we use start_link instead of this, the clients processes get an 
        	%exit signal on connection error, when they are expecting to get
        	%a {socket_error,Reason} response. In this way, the API is 
        	%compatible with exmpp_tcp
            {Ref, undefined};
        {error, Reason} ->
            erlang:throw({socket_error, Reason})
    end.

close(Ref, _) ->
    gen_server:call(Ref, close).

send(Ref, XMLPacket) ->
    %% TODO: document_to_binary to reduce memory consumption
    String = exmpp_xml:document_to_list(XMLPacket),
    gen_server:call(Ref, {send, String}).

%% -- gen_server implementation --

init([ClientPid, StreamRef, Host, Port]) ->
	SetOptsModule = 
			case check_new_ssl() of 
			   true -> ssl;
			   false -> inet
			end,		
			
    Opts = [{packet,0}, binary, {active, once}, {reuseaddr, true}],
    case ssl:connect(Host, Port, Opts, 30000) of
        {ok, Socket} ->
            {ok, #state{socket = Socket, stream_ref = StreamRef,
			client_pid = ClientPid,
			set_opts_module=SetOptsModule}};
        {error,Error} ->
            {stop,Error}
    end.
    
handle_call(close, From, State) ->
    Result = ssl:close(State#state.socket),
    gen_server:reply(From, Result),
    {stop, normal, State};

handle_call({send, Data}, From, State) ->
    case ssl:send(State#state.socket, Data) of
        ok ->
            {reply, ok, State};
        Error ->
            gen_server:reply(From, {error, send_failed}),
            {stop, Error, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({ssl, Socket, Data}, #state{socket = Socket} = State) ->
    {ok, NewStreamRef} = exmpp_xmlstream:parse(State#state.stream_ref, Data),
    
    Module=State#state.set_opts_module,
    Module:setopts(Socket, [{active, once}]),
    {noreply, State#state{stream_ref = NewStreamRef}};

handle_info({ssl_error, Socket, _Reason}, #state{socket = Socket} = State) ->
    {noreply, State};

handle_info({ssl_closed, Socket},
	    #state{socket = Socket, client_pid=ClientPid} = State) ->
    gen_fsm:sync_send_all_state_event(ClientPid, tcp_closed),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
    

%% In R12, inet:setopts/2 doesn't accept the new ssl sockets
check_new_ssl() ->
	case erlang:system_info(version) of
        [$5,$.,Maj] when Maj < $6  ->
            false;
        [$5,$.,Maj, $.,_Min] when ( Maj < $6 ) ->
            false;
        _  ->
            true
    end.
