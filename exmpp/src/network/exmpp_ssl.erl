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

-export([connect/2, send/2, close/1]).

-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {socket, stream_ref}).

%% -- client interface --

%% Connect to XMPP server
%% Returns:
%% Ref | {error, Reason}
%% Ref is a pid.
connect(StreamRef, {Host, Port}) ->
    case gen_server:start_link(?MODULE, [StreamRef, Host, Port], []) of
        {ok, Ref} ->
            Ref;
        {error, Reason} ->
            erlang:throw({socket_error, Reason})
    end.

close(Ref) ->
    gen_server:call(Ref, close).

send(Ref, XMLPacket) ->
    %% TODO: document_to_binary to reduce memory consumption
    String = exmpp_xml:document_to_list(XMLPacket),
    gen_server:call(Ref, {send, String}).

%% -- gen_server implementation --

init([StreamRef, Host, Port]) ->
    Opts = [{packet,0}, binary, {active, true}, {reuseaddr, true}],
    case ssl:connect(Host, Port, Opts, 30000) of
        {ok, Socket} ->
            {ok, #state{socket = Socket, stream_ref = StreamRef}};
        Error ->
            {stop, Error}
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
    {noreply, State#state{stream_ref = NewStreamRef}};

handle_info({ssl_error, Socket, Reason}, #state{socket = Socket} = State) ->
    {noreply, State};

handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.
    
