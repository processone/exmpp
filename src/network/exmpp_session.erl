%% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> puts together the mechanism
%% to connect to an XMPP server, using various authentication
%% mechanisms and network layers.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 
%% TODO: Rewrite the following text into a comprehensive documentation.
%%   explain priority matching of iq query reply. Illustration with echo_client.
%%   Illustration with an Erlang/OTP example.
%%
%% TODO: - manage timeouts
%%       - Callback should not be module, but anonymous or named
%%       functions
%%       - Do function callback need to have priority ?
%%
%% Currently thinking about a design purely based on sending back messages to
%% the client process: Would allow selection of the order of packets (whic
%% seems more powerful)
%% Sending a packet is async and return the packet id
%% If this is an IQ, the next reply can be a blocking receive on an IQ result
%% with the same refid.
%% It could be a generic receive, getting packets in order.

-module(exmpp_session).
-behaviour(gen_fsm).

%% XMPP Session API:
-export([start/0, start_link/0, start_debug/0, stop/1]).
-export([auth_basic/3, auth_basic_digest/3,
	 connect_TCP/3, register_account/2, login/1,
	 send_packet/2]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
	 terminate/3]).

%% States
-export([setup/3, wait_for_stream/2, wait_for_stream/3,
	 stream_opened/3,
	 wait_for_legacy_auth_method/2,
	 wait_for_auth_result/2,
	 wait_for_register_result/2,
	 logged_in/2, logged_in/3
	]).

-include("exmpp.hrl").
-include("exmpp_client.hrl").

-record(state, {
  auth_method = undefined,
  client_pid,
  connection = exmpp_tcp,
  connection_ref,
  stream_ref,
  stream_id = false, %% XMPP StreamID (Used for digest_auth)
  from_pid           %% Use by gen_fsm to handle postponed replies
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
%% Start the session (used to get a reference):
start() ->
    case gen_fsm:start(?MODULE, [self()], []) of
	{ok, PID} -> PID;
	{error, Reason} -> erlang:error({error, Reason})
    end.
%% Start the session (used to get a reference):
start_link() ->
    gen_fsm:start_link(?MODULE, [self()], []).    

%% Start the session in debug mode
%% (trace events)
start_debug() ->
    gen_fsm:start(?MODULE, [], [{debug,[trace]}]).

%% Close session and disconnect
stop(Session) ->
    %% TODO
    ok.

%% Set authentication mode to basic (password)
auth_basic(Session, JID, Password)
when pid(Session),
list(Password) ->
    case exmpp_jid:is_jid(JID) of
	false -> erlang:error({incorrect_jid,JID});
	true ->
	    Auth = {basic, password, JID, Password},
	    gen_fsm:sync_send_event(Session, {set_auth, Auth})
    end.

%% Set authentication mode to basic (digest)
auth_basic_digest(Session, JID, Password)
when pid(Session),
list(Password) ->
    case exmpp_jid:is_jid(JID) of
	false -> erlang:error({incorrect_jid,JID});
	true ->
	    Auth = {basic, digest, JID, Password},
	    gen_fsm:sync_send_event(Session, {set_auth, Auth})
    end.

%% Initiate standard TCP XMPP server connection
%% Returns StreamId (String)
connect_TCP(Session, Server, Port) 
when pid(Session),
list(Server),
integer(Port) ->
    case gen_fsm:sync_send_event(Session, {connect_tcp, Server, Port}) of
	Error when tuple(Error) -> erlang:throw(Error);
	StreamId -> StreamId
    end.

%% Trying to add the session user with inband registration
%% Returns ok
register_account(Session, Password) ->
    case gen_fsm:sync_send_event(Session, {register_account, Password}) of
	ok -> ok;
	Error when tuple(Error) -> erlang:throw(Error)
    end.

%% Login session user
%% Returns ok
login(Session) when pid(Session) ->
    case gen_fsm:sync_send_event(Session, {login}) of
	ok -> ok;
	Error when tuple(Error) -> erlang:throw(Error)
    end.

%% Send any exmpp formatted packet
send_packet(Session, Packet) when pid(Session) ->
    case gen_fsm:sync_send_event(Session, {send_packet, Packet}) of
	Error when tuple(Error) -> erlang:throw(Error);
        Id -> Id
    end.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([Pid]) ->
    exmpp_stringprep:start(),
    %% TODO: Init random numbers generator ?
    {ok, setup, #state{client_pid=Pid}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(Reason, StateName, #state{connection_ref = undefined,
				    stream_ref = undefined,
				    from_pid=From}) ->
    reply(Reason, From),
    ok;
terminate(Reason, StateName, #state{connection_ref = undefined,
				    stream_ref = StreamRef,
				    from_pid=From}) ->
    reply(Reason, From),
    exmpp_xmlstream:stop(StreamRef),
    ok;
terminate(Reason, StateName, #state{connection_ref = ConnRef,
				    connection = Module,
				    stream_ref = undefined,
				    from_pid=From}) ->
    reply(Reason, From),
    Module:close(ConnRef),
    ok;
terminate(Reason, StateName, #state{connection_ref = ConnRef,
				    connection = Module,
				    stream_ref = StreamRef,
				    from_pid=From}) ->
    reply(Reason, From),
    Module:close(ConnRef),
    exmpp_xmlstream:stop(StreamRef),
    ok.

%% Send gen_fsm reply if needed
reply(_Reply, undefined) ->
    ok;
reply(Reply, From) when pid(From) ->
    gen_fsm:reply(From, Reply);
reply(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% States
%%====================================================================
%% ---------------------------
%% Setup state: Configuration

%% Define JID and authentication method
setup({set_auth, Auth}, _From, State) when tuple(Auth) ->
    {reply, ok, setup, State#state{auth_method=Auth}};
setup({connect_tcp, Host, Port}, From, State) ->
    Domain = get_domain(State#state.auth_method),
    Module = exmpp_tcp,
    {ok, StreamRef} = start_parser(),
    try Module:connect(StreamRef, {Host, Port}) of
	ConnRef ->
	    %% basic (legacy) authent: we do not use version 1.0 in stream:
	    ok = Module:send(ConnRef,
			     exmpp_client_stream:opening([{to, Domain}])),
	    {next_state, wait_for_stream, State#state{connection = Module,
						      connection_ref = ConnRef,
						      stream_ref = StreamRef,
						      from_pid = From}}
    catch
	throw:Error ->
	    gen_fsm:reply(From, Error),
	    exmpp_xmlstream:stop(StreamRef),
	    %% We do not stop here, because the developer might want to 
	    %% start a connection using another transport
	    {next_state, setup, State#state{stream_ref = StreamRef,
					    from_pid = From}}
    end;
setup({presence, _Status, _Show}, _From, State) ->
    {reply, {error, not_connected}, setup, State};
setup(UnknownMessage, _From, State) ->
    {reply, {error, unallowed_command}, setup, State}.

%% ---------------------------
%% Stream negociation:

%% TODO: Defines should probably be refactored with the other parts of
%% exmpp.

%% Standard opening stream:
-define(stream,
	#xmlstreamstart{element=#xmlnselement{
          ns='http://etherx.jabber.org/streams',
          name=stream}}).
%% Standard stream error:
-define(streamerror,
	#xmlstreamelement{element=#xmlnselement{
          ns='http://etherx.jabber.org/streams',
          name=error,
          children=[#xmlnselement{name=Reason}]}}).

%% Special stream error: disconnected
-define(streamdisconnected,
        #xmlstreamelement{element=#xmlnselement{
          ns='http://etherx.jabber.org/streams',
          name=error,
          children=[#xmlcdata{cdata=  <<"Disconnected">> }]}}).

%% Extract IQElement from IQ 
-define(iq,
	#xmlstreamelement{
	  element=#xmlnselement{name=iq}=IQElement}).

%% We cannot receive API call in this state
wait_for_stream(Event, From, State) ->
    {reply, {error, busy_connecting_to_server}, wait_for_stream, State}.
%% TODO: Check that we receive a client stream. Need change in the
%% parsing library.
wait_for_stream(Start = ?stream, State = #state{connection = Module,
						connection_ref = ConnRef,
						auth_method = Auth,
						from_pid = From}) ->
    %% Get StreamID
    StreamId = exmpp_xml:get_attribute(Start#xmlstreamstart.element, id),
    gen_fsm:reply(From, StreamId),
    {next_state, stream_opened, State#state{from_pid=undefined,
					    stream_id = StreamId}}.

%% ---------------------------
%% Between stream opening and session opening

%% Supported user commands at this stage:
%% login and register
stream_opened({login}, _From,State=#state{auth_method=undefined}) ->
    {reply, {error, auth_method_undefined}, stream_opened, State};
stream_opened({login}, From, State=#state{connection = Module,
					  connection_ref = ConnRef,
					  auth_method=Auth}) ->
    %% Retrieve supported authentication methods:
    %% TODO: Do different thing if we use basic or SASL auth
    %% For now, we consider everything is legacy (basic)
    Domain = get_domain(Auth),
    Username = get_username(Auth),
    Module:send(ConnRef,
 		exmpp_client_legacy_auth:request_with_user(Domain, Username)),
    {next_state, wait_for_legacy_auth_method, State#state{from_pid=From}};
stream_opened({register_account, Password}, From,
	      State=#state{connection = Module,
			   connection_ref = ConnRef,
			   auth_method=Auth}) ->
    Username = get_username(Auth),
    Module:send(ConnRef,
		exmpp_client_register:register_account([{username, Username},
                                                        {password, Password}])),
    {next_state, wait_for_register_result, State#state{from_pid=From}};

%% We can define update login informations after we are connected to
%% the XMPP server:
%% Define JID and authentication method
stream_opened({set_auth, Auth}, _From, State) when tuple(Auth) ->
    {reply, ok, stream_opened, State#state{auth_method=Auth}};
stream_opened({presence, _Status, _Show}, _From, State) ->
    {reply, {error, not_logged_in}, setup, State}.


%% Reason comes from streamerror macro
wait_for_legacy_auth_method(?iq, State = #state{connection = Module,
						connection_ref = ConnRef,
						auth_method = Auth,
						stream_id = StreamId}) ->
    Username = get_username(Auth),
    Password = get_password(Auth),
    Resource = get_resource(Auth),
    Method = get_method(Auth),
    case check_auth_method(Method, IQElement) of
	ok -> 
	    case do_auth(Method, ConnRef, Module, Username, Password, Resource,
                         StreamId) of
		ok ->
		    {next_state, wait_for_auth_result, State};
		Error ->
		    {stop, Error, State}
	    end;
	{error, Reason} ->
	    {stop, {error, Reason}, State}
    end;
wait_for_legacy_auth_method(?streamerror, State) ->
    {stop, {error, Reason}, State}.

%% TODO: We should be able to match on iq type directly on the first
%% level record
wait_for_auth_result(?iq, State = #state{from_pid=From}) ->
    case exmpp_xml:get_attribute(IQElement, type) of
 	"result" ->
            gen_fsm:reply(From, ok),	     
            {next_state, logged_in, State#state{from_pid=undefined}};
        "error" ->
            Reason = exmpp_error:get_reason(IQElement),
            gen_fsm:reply(From, {auth_error, Reason}),
            {next_state, stream_opened, State#state{from_pid=undefined}}
    end.

%% Note: We do not get the field received from server to perform register
%% TODO: The API should be flexible to adapt to server
%% requirements. Check that a client can get the list of fields and
%% override this simple method of registration.
wait_for_register_result(?iq, State = #state{from_pid=From}) ->
    case exmpp_xml:get_attribute(IQElement, type) of
 	"result" ->
            gen_fsm:reply(From, ok),	     
            {next_state, stream_opened, State#state{from_pid=undefined}};
        "error" ->
            Reason = exmpp_error:get_reason(IQElement),
            gen_fsm:reply(From, {register_error, Reason}),
            {next_state, stream_opened, State#state{from_pid=undefined}}
    end.    

%% Used to match a presence packet in stream.
-define(presence,
	#xmlstreamelement{
	  element=#xmlnselement{name=presence, attrs=Attrs}=PresenceElement}).
%% Used to match a message packet in stream
-define(message,
	#xmlstreamelement{
	  element=#xmlnselement{name=message, attrs=Attrs}=MessageElement}). 
%% To match an XMLNSElement of type Iq:
-define(iqattrs, #xmlnselement{name=iq, attrs=Attrs}=IQElement).
%% To match either presence or message
-define(elementattrs, #xmlnselement{attrs=Attrs}=Element).

%% ---
%% Send packets
%% TODO: 
%% If the packet is an iq set or get:
%% We check that there is a valid id and store it to match the reply
logged_in({send_packet, ?iqattrs}, From,
	  State = #state{connection = Module,
			 connection_ref = ConnRef}) ->
    Type = exmpp_xml:get_attribute_from_list(Attrs, type),
    Id = case Type of 
             "error" ->
                 {Attrs2, PacketId} = check_id(Attrs),
                 Module:send(ConnRef, IQElement#xmlnselement{attrs=Attrs2}),
                 PacketId;
             "result" -> 
                 {Attrs2, PacketId} = check_id(Attrs),
                 Module:send(ConnRef, IQElement#xmlnselement{attrs=Attrs2}),
                 PacketId;
             "set" ->
                 {Attrs2, PacketId} = check_id(Attrs),
                 Module:send(ConnRef, IQElement#xmlnselement{attrs=Attrs2}),
                 PacketId;
             "get" ->
                 {Attrs2, PacketId} = check_id(Attrs),
                 Module:send(ConnRef, IQElement#xmlnselement{attrs=Attrs2}),
                 PacketId
         end,
    {reply, Id, logged_in, State};	  
logged_in({send_packet, ?elementattrs}, _From,
	  State = #state{connection = Module,
			 connection_ref = ConnRef}) ->
    {Attrs2, Id} = check_id(Attrs),
    Module:send(ConnRef, Element#xmlnselement{attrs=Attrs2}),
    {reply, Id, logged_in, State}.

%% ---
%% Receive packets
%% When logged in we dispatch the event we receive
%% Dispatch incoming presence packets
logged_in(?presence,
	  State = #state{connection = Module,
			 connection_ref = ConnRef}) ->
    process_presence(State#state.client_pid, Attrs, PresenceElement),
    {next_state, logged_in, State};
%% Dispatch incoming messages
logged_in(?message, State = #state{connection = Module,
				   connection_ref = ConnRef}) ->
    process_message(State#state.client_pid, Attrs, MessageElement),
    {next_state, logged_in, State};
%% Dispach IQs from server
logged_in(?iq, State) ->
    %% TODO: Alarm log, ignore IQ or send event to be controled by the client 
    %io:format("IQ: ~p~n", [IQElement]),
    {next_state, logged_in, State};
%% Process unexpected packet
logged_in(_Packet, State) ->
    %% log it or do something better
    %io:format("!!!ALERT!!! Unknown packet:~p~p~n", [_Packet, State]),
    {next_state, logged_in, State}.

%% TODO:
%% Handle disconnections
%% Connection replaced.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Authentication
%% digest auth will fail if we do not have streamid
do_auth(password, ConnRef, Module, Username, Password, Resource, _StreamId) ->
    Module:send(ConnRef,
		exmpp_client_legacy_auth:password(Username, Password, 
						  Resource));
do_auth(digest, ConnRef, Module, Username, Password, Resource, StreamId) 
when list(StreamId) ->
    Module:send(ConnRef,
		exmpp_client_legacy_auth:password_digest(StreamId,
							 Username,
							 Password,
							 Resource));
%% In this case StreamId can be false
do_auth(digest, ConnRef, Module, Username, Password, Resource, StreamId) 
when atom(StreamId) ->
    {auth_error, no_streamid_for_digest_auth}.

%% Extraction functions

%% Extract domain from Auth Method
get_domain({basic, _Method, JID, _Password}) when record(JID, jid) ->
    JID#jid.domain.
get_username({basic, _Method, JID, _Password}) when record(JID, jid) ->
    JID#jid.node.
get_resource({basic, _Method, JID, _Password}) when record(JID, jid) ->
    JID#jid.resource.
get_password({basic, _Method, _JID, Password}) when list(Password) ->
    Password.
get_method({basic, Method, _JID, _Password}) when atom(Method) ->
    Method.

%% Parsing functions

%% Define parser options
%% No compatibility mode: We use all the nice optimisation of exmpp:
-define(PARSER_OPTIONS,
	[namespace,
	 name_as_atom,
	 ns_check,                        
	 names_check,                                                          
	 attrs_check,                                                          
	 no_maxsize]).

%% Start parser and return stream reference
start_parser() ->
    exmpp_xmlstream:start({gen_fsm, self()}, ?PARSER_OPTIONS).

%% Authentication functions
check_auth_method(Method, IQElement) ->
    %% Check auth method if we have the IQ result
    case exmpp_xml:get_attribute(IQElement, type) of
	"result" ->
	    check_auth_method2(Method, IQElement);
	_ ->
	    {error, not_auth_method_result}
    end.
check_auth_method2(Method, IQElement) ->
    QueryElement = exmpp_xml:get_element_by_name(IQElement,
						 'jabber:iq:auth',
						 'query'),
    case exmpp_xml:get_element_by_name(QueryElement,
				       'jabber:iq:auth',
				       Method) of
	false ->
	    {error, no_supported_auth_method};
	_ ->
	    ok
    end.

%% Packet processing functions
process_presence(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, type, "available"),
    Who = get_attribute_value(Attrs, from, ""),
    Id = get_attribute_value(Attrs, id, ""),
    ClientPid ! #received_packet{packet_type = presence,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

process_message(ClientPid, Attrs, Packet) ->
    Type = get_attribute_value(Attrs, type, "normal"),
    Who = get_attribute_value(Attrs, from, ""),
    Id = get_attribute_value(Attrs, id, ""),
    ClientPid ! #received_packet{packet_type = message,
                                 type_attr = Type,
                                 from = Who,
                                 id = Id,
                                 raw_packet = Packet}.

%% Add a packet ID is needed:
%% Check that the attribute list has defined an ID.
%% If no ID has been defined, add a packet id to the list of attributes
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
check_id(Attrs) ->
    case exmpp_xml:get_attribute_from_list(Attrs, id) of
	"" -> 	
	    Id = "session-"++integer_to_list(random:uniform(65536 * 65536)),
	    {exmpp_xml:set_attribute_in_list(Attrs, id, Id), Id};
        Id -> {Attrs, Id}
    end.
    
%% Try getting a given atribute from a list of xmlattr records
%% Return default value if attribute is not found
get_attribute_value(Attrs, Attr, Default) ->
    case exmpp_xml:get_attribute_node_from_list(Attrs, Attr) of
        false -> Default;
        #xmlattr{value=Value} -> Value
    end. 
