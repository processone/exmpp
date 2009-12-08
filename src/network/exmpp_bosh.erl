%% Copyright ProcessOne 2006-2009. All Rights Reserved.
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

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> manages XMPP over HTTP connection
%% according to the BOSH protocol (XEP-0124: Bidirectional-streams Over
%% Synchronous HTTP)
%%
%% <p>
%% This module is not intended to be used directly by client developers.
%% </p>
%%
%%
%% @pablo  IMPORTANT
%%         This transport requires the lhttpc library, it doesn't work
%%         with OTP's http client. See make_request/2

-module(exmpp_bosh).

%-include_lib("exmpp/include/exmpp.hrl").
-include("exmpp.hrl").

%% Behaviour exmpp_gen_transport ?
-export([connect/3,  send/2, close/2, reset_parser/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(CONTENT_TYPE, "text/xml; charset=utf-8").
-define(HOLD, "2").
-define(VERSION, "1.8").
-define(WAIT, "3600").

-record(state, {bosh_url="",
		domain="",
		sid = <<>>,
		rid = 0,
		auth_id = <<>>,
		client_pid,
		stream_ref,
        max_requests,
        new = true,
        open_connections = [] %% http connections currently open and waiting for server response
	       }).

reset_parser(Pid) ->
    gen_server:call(Pid,reset_parser).

connect(ClientPid, StreamRef, {URL, Domain, _Port}) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [ClientPid, StreamRef, URL, Domain], []),
    {Pid, Pid}.

send(Pid, Packet) ->
    gen_server:cast(Pid, {send, Packet}).

close(Pid, _) ->
    gen_server:call(Pid, stop).

%% don't do anything on init. We establish the connection when the stream start 
%% is sent
init([ClientPid, StreamRef, URL, Domain]) ->
    Rid = 324545,
    State = #state{bosh_url = URL,
            domain = Domain,
            rid = Rid,
            client_pid = ClientPid,
            stream_ref = StreamRef
            },
     {ok, State}.

%% reset the connection. We send here a fake stream response to the client to.
%% TODO: check if it is not best to do this in do_send/2 
handle_call(reset_parser, _From, State) ->
    #state{stream_ref = Stream,
           bosh_url = URL,
           rid = Rid,
           sid = Sid,
           auth_id = AuthID,
           open_connections = Open,
           domain = Domain} =  State,
    Ref = make_request_async(URL, restart_stream_msg(Sid, Rid, Domain)),
	StreamStart =
		["<?xml version='1.0'?><stream:stream xmlns='jabber:client'"
		" xmlns:stream='http://etherx.jabber.org/streams' version='1.0'"
		" from='" , Domain , "' id='" , AuthID , "'>"],
    NewStreamRef = exmpp_xmlstream:reset(Stream),
    {ok, NewStreamRef2} = exmpp_xmlstream:parse(NewStreamRef, StreamStart),
    {reply, ok, State#state{rid = Rid +1,
                            new = false,
                            stream_ref = NewStreamRef2, 
                            open_connections = [Ref | Open]}};



handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Packet}, State) ->
    do_send(Packet, State);
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({http_response, Pid, {ok, Resp}}, #state{open_connections = Open, stream_ref = Stream} = State) ->
    case lists:member(Pid, Open) of
        false ->
            exit({unknown_http_process, Pid, Open});
        true ->
            NewState = if
                length(Open) =:= 1  andalso State#state.new =:= false ->   % for some reason we can't do this before authentication
                    #state{sid = Sid, rid = Rid} = State,
                    Ref = make_request_async(State#state.bosh_url, empty_msg(Sid, Rid)),
                    State#state{open_connections = [Ref | lists:delete(Pid, Open)], rid = Rid +1};
                true ->
                    State#state{open_connections = lists:delete(Pid, Open)} 
            end,
            [#xmlel{name=body} = BodyEl] = exmpp_xml:parse_document(Resp),
            Events = [{xmlstreamelement, El} || El <- exmpp_xml:get_child_elements(BodyEl)],
            exmpp_xmlstream:send_events(Stream, Events),
            {noreply, NewState}
     end;
                    
handle_info(_Info, State) ->
    io:format("Got unknown info ~p \n", [_Info]),
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_Old, State, _Extra) ->
    {ok, State}.


make_request_async(URL, Body) ->
    BoshMngr = self(),
    spawn_link(fun() ->
        BoshMngr ! {http_response, self(), make_request(URL, Body)}
    end).
        
make_request(URL, Body) ->
%    TODO:  things don't work well with the http client included in OTP. 
%    Don't know why, but request get stuck.  It works ok with lhttpc.
%    I have to investigate it further, seems some problem with 
%    http  connection pipelining/persistent.  For now I want to keep
%    this as simple as possible.
%    Request =  {URL, [], "text/xml; charset=utf-8", Body},
%    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Resp}} = http:request(post, Request, [], [{sync, true}]),

    {ok, {{200, _Reason}, _Headers, Resp}} = lhttpc:request(URL, 'post', [{"Content-Type", ?CONTENT_TYPE}], Body, infinity),
    {ok, Resp}.

%% after stream restart, we must not sent this to the connection manager. The response is got in reset call
do_send(#xmlel{ns=?NS_XMPP, name='stream'}, #state{new = false} = State) ->
	{noreply, State};

% we start the session with the connection manager here.
do_send(#xmlel{ns=?NS_XMPP, name='stream'}, State) ->
    #state{ bosh_url = URL,
            domain = Domain, 
            stream_ref = StreamRef,
            rid = Rid} = State,

    {ok, Resp} = make_request(URL, create_session_msg(Rid, Domain, 10, 1)),

    [#xmlel{name=body} = BodyEl] = exmpp_xml:parse_document(Resp),
    SID = exmpp_xml:get_attribute_as_binary(BodyEl, sid, undefined),
    AuthID = exmpp_xml:get_attribute_as_binary(BodyEl,authid,undefined),
    Requests = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,requests,undefined)),
    %Inactivity = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,inactivity,undefined)),
    Requests = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,requests,undefined)),
    Events = [{xmlstreamelement, El} || El <- exmpp_xml:get_child_elements(BodyEl)],

    % first return a fake stream response, then anything found inside the <body/> element (possibly nothing)
	StreamStart =
		["<?xml version='1.0'?><stream:stream xmlns='jabber:client'"
		" xmlns:stream='http://etherx.jabber.org/streams' version='1.0'"
		" from='" , Domain , "' id='" , AuthID , "'>"],
    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, StreamStart),
    exmpp_xmlstream:send_events(StreamRef, Events),
	{noreply, State#state{stream_ref = NewStreamRef, 
                          rid = Rid +1, 
                          sid = SID,
                          open_connections = [],
                          auth_id = AuthID}};

do_send(Packet, State) ->
    case length(State#state.open_connections) of
        N when N >=  State#state.max_requests ->
            %% TODO: here we must enqueue the stanza for latter delivery, we shouldn't crash.
            exit({max_requests_reached, N, State#state.max_requests});
        _ ->
            Ref = make_request_async(State#state.bosh_url, 
                    stanzas_msg(State#state.sid,
                                State#state.rid,
                                exmpp_xml:document_to_iolist(Packet))),
            {noreply, State#state{open_connections = [Ref | State#state.open_connections],
                        rid = State#state.rid +1}}
     end.
   
    


create_session_msg(Rid, To, Wait, Hold) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind'"
       " content='text/xml; charset=utf-8'",
       " ver='1.8'"
       " to='", To, "'",
       " rid='", integer_to_list(Rid), "'" 
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " xmpp:version='1.0'",
       " wait='", integer_to_list(Wait), "'"
       " hold='", integer_to_list(Hold), "'/>"].

stanzas_msg(Sid, Rid, Text) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'" 
       " sid='", Sid, "'>", Text, "</body>"].


empty_msg(Sid, Rid) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'" 
       " sid='", Sid, "'></body>"].

restart_stream_msg(Sid, Rid, Domain) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " xmpp:restart='true'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " to='", Domain, "'",
       "/>"].
