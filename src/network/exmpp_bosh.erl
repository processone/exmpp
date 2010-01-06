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
%% TODO Pablo:  IMPORTANT
%%         This transport requires the lhttpc library, it doesn't work
%%         with OTP's http client. See make_request/2

-module(exmpp_bosh).

%-include_lib("exmpp/include/exmpp.hrl").
-include("exmpp.hrl").

%% Behaviour exmpp_gen_transport ?
-export([connect/3,  send/2, close/2, reset_parser/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(CONTENT_TYPE, "text/xml; charset=utf-8").
-define(VERSION, "1.8").
-define(WAIT, 60). %1 minute
-define(HOLD, 1). %only 1 request pending


-record(state, {
        parsed_bosh_url, 
           % {Host::string(), Port:integer(), Path::string(), Ssl::boolean()}
          % can be obtained by lhttpc_lib:parse_url/1
		domain="",
		sid = <<>>,
		rid = 0,
		auth_id = <<>>,
		client_pid,
		stream_ref,
        max_requests,
        polling,  %% This attribute specifies the shortest allowable polling interval (in seconds)
        queue, %% stanzas that have been queued because we reach the limit of requets or the polling
        last_request_timestamp,
        new = true,
        max_inactivity,
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
    inets:start(),
    exmpp_stringprep:start(),

    {A,B,C} = now(),
    random:seed(A,B,C),
    Rid = 1000 + random:uniform(100000),
    ParsedUrl = lhttpc_lib:parse_url(URL),
    State = #state{parsed_bosh_url = ParsedUrl,
            domain = Domain,
            rid = Rid,
            client_pid = ClientPid,
            queue = queue:new(),
            stream_ref = exmpp_xmlstream:set_wrapper_tagnames(StreamRef, [body])
            },
     {ok, State}.

%% reset the connection. We send here a fake stream response to the client to.
%% TODO: check if it is not best to do this in do_send/2 
handle_call(reset_parser, _From, State) ->
    #state{stream_ref = Stream,
           sid = Sid,
           rid = Rid,
           auth_id = AuthID,
           domain = Domain} =  State,
    NewState = make_request_async(State, restart_stream_msg(Sid, Rid, Domain)),
	StreamStart =
		["<?xml version='1.0'?><stream:stream xmlns='jabber:client'"
		" xmlns:stream='http://etherx.jabber.org/streams' version='1.0'"
		" from='" , Domain , "' id='" , AuthID , "'>"],             
    NewStreamRef = exmpp_xmlstream:reset(Stream),
    {ok, NewStreamRef2} = exmpp_xmlstream:parse(NewStreamRef, StreamStart),
    {reply, ok, NewState#state{new = false,
                            stream_ref = NewStreamRef2}, hibernate};



handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Packet}, State) ->
    do_send(Packet, State);
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    NewState = make_request_async(State),
    {noreply, NewState, hibernate};

    
handle_info({http_response, Pid, {ok, Resp}}, #state{open_connections = Open, stream_ref = Stream} = State) ->
    case lists:member(Pid, Open) of
        false ->
            exit({unknown_http_process, Pid, Open});
        true ->
            NewState = if
                length(Open) =:= 1  andalso State#state.new =:= false ->   % for some reason we can't do this before authentication
                    New  = make_request_async(State),
                    New#state{open_connections = lists:delete(Pid, New#state.open_connections)};
                true ->
                    State#state{open_connections = lists:delete(Pid, Open)} 
            end,
             {ok, NewStream} = exmpp_xmlstream:parse(Stream, Resp),
            {noreply, NewState#state{stream_ref = NewStream}, hibernate}
     end;
                    
handle_info(_Info, State) ->
    {noreply, State, hibernate}.
terminate(_Reason, _State) ->
    ok.
code_change(_Old, State, _Extra) ->
    {ok, State}.


make_request_async(#state{sid = Sid, rid = Rid, open_connections = Open, queue = Queue, parsed_bosh_url=URL} = State) ->
    BoshMngr = self(),
    Pid = spawn_link(fun() ->
            StanzasText = [exmpp_xml:document_to_iolist(I) || I <- queue:to_list(Queue)],
            Body = stanzas_msg(Sid, Rid, StanzasText),
            BoshMngr ! {http_response, self(), make_request(URL, Body)}
    end),
    State#state{open_connections = [Pid | Open],  rid = State#state.rid +1, queue = queue:new()}.

make_request_async(State = #state{sid = Sid, rid = Rid, parsed_bosh_url = URL, open_connections=Open, queue = Queue}, Packet) when is_record(Packet, xmlel) ->
    BoshMngr = self(),
    Pid = spawn_link(fun() ->
            StanzasText = [exmpp_xml:document_to_iolist(I) || I <- queue:to_list(queue:in(Packet,Queue))],
            Body = stanzas_msg(Sid, Rid, StanzasText),
            BoshMngr ! {http_response, self(), make_request(URL, Body)}
    end),
    State#state{open_connections = [Pid | Open],  rid = State#state.rid +1, queue = queue:new() };
make_request_async(State = #state{parsed_bosh_url = URL, open_connections=Open}, Body) ->
    BoshMngr = self(),
    Pid = spawn_link(fun() ->
            BoshMngr ! {http_response, self(), make_request(URL, Body)}
    end),
    State#state{open_connections = [Pid | Open],  rid = State#state.rid +1}.
        
make_request({Host, Port, Path, Ssl}, Body) ->
%    TODO:  things don't work well with the http client included in OTP. 
%    Don't know why, but request get stuck.  It works ok with lhttpc.
%    I have to investigate it further, seems some problem with 
%    http  connection pipelining/persistent.  For now I want to keep
%    this as simple as possible.
%    Request =  {URL, [], "text/xml; charset=utf-8", Body},
%    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Resp}} = http:request(post, Request, [], [{sync, true}]),

    {ok, {{200, _Reason}, _Headers, Resp}} = lhttpc:request(Host, Port, Ssl, Path, 'post', [{"Content-Type", ?CONTENT_TYPE}], Body, infinity, []),

    {ok, Resp}.

%% after stream restart, we must not sent this to the connection manager. The response is got in reset call
do_send(#xmlel{ns=?NS_XMPP, name='stream'}, #state{new = false} = State) ->
	{noreply, State};

% we start the session with the connection manager here.
do_send(#xmlel{ns=?NS_XMPP, name='stream'}, State) ->
    #state{ parsed_bosh_url = ParsedURL,
            domain = Domain, 
            stream_ref = StreamRef,
            rid = Rid} = State,

    {ok, Resp} = make_request(ParsedURL, create_session_msg(Rid, Domain, ?WAIT, ?HOLD)),
   
    [#xmlel{name=body} = BodyEl] = exmpp_xml:parse_document(Resp),
    SID = exmpp_xml:get_attribute_as_binary(BodyEl, sid, undefined),
    AuthID = exmpp_xml:get_attribute_as_binary(BodyEl,authid,undefined),
    Requests = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,requests,undefined)),
    Inactivity = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,inactivity,undefined)) * 1000, %sec -> millisecond
    Polling = list_to_integer(exmpp_xml:get_attribute_as_list(BodyEl,polling,undefined)) * 1000, %sec -> millisecond
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
                          max_requests = Requests,
                          max_inactivity = Inactivity, %%TODO: not used 
                          polling = Polling,
                          auth_id = AuthID}};

do_send(Packet, State) ->
    Now = now(),
    Result  = case length(State#state.open_connections) of
     %   TODO: check if/when this situation can actually happen.  The spec says (http://xmpp.org/extensions/xep-0124.html#overactive)
     %          that we must not send faster than the polling interval *ONLY* if we send empty requests. Here we never sent empty requests.
     %
     %   N when (N == State#state.max_requests - 1 ) ->   %% check if request aren't too fast http://xmpp.org/extensions/xep-0124.html#overactive
     %       Interval = timer:now_diff(Now, State#state.last_request_timestamp) / 1000,  % micro -> millisec
     %       case Interval < State#state.polling of
     %           true ->
     %               {queue, round(State#state.polling - Interval)}; 
     %           false ->
     %               send
     %       end;
        N when N ==  State#state.max_requests ->  %% check that we don't open more request than allowed http://xmpp.org/extensions/xep-0124.html#overactive
            {queue, undefined};
        _X ->
            send
     end,
     case Result of
         send ->
              NewState= make_request_async(State,Packet), 
              {noreply, NewState#state{last_request_timestamp=Now}, hibernate};
         {queue, Time} ->
                Queue = State#state.queue,
                NewQueue =  queue:in(Packet, Queue),
                case Time of
                     undefined ->
                        ok;
                     _ ->
                         case queue:is_empty(Queue) of  %% first message queue, no timeout was setup yet
                               true ->
                                    erlang:send_after(Time, self(), timeout);
                               false ->
                                    ok
                         end
               end,
               {noreply, State#state{queue = NewQueue}}
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

restart_stream_msg(Sid, Rid, Domain) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " xmpp:restart='true'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " to='", Domain, "'",
       "/>"].

