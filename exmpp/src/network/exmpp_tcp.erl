% $Id: exmpp.erl 214 2007-03-06 16:44:02Z cromain $

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> implement the TCP layer needed to
%% connect to an XMPP server.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

-module(exmpp_tcp).
-behaviour(gen_fsm).

%% XMPP api
-export([start/0, start/1, start/2, start/3,
	 stop/1,
	 set_login_information/3,
	 set_login_information/4,
	 set_host/2,
	 set_host/3,
	 connect/1,
	 register_user/3,
	 set_callback_module/2,
	 message/4, message/5,
	 send/2,
	 subscribe/2
	]).

%% XMPP API: Those function are used to tweak the XMPP library
%% behaviour for automatic IQ response:
-export([set_client_info/3]).

%% FSM states
-export([unconfigured/3,
	 ready_to_connect/3,
	 wait_for_stream/2,
	 wait_for_authentication_method/2,
	 wait_for_authentication_result/2,
	 wait_for_registration_result/2,
	 wait_for_element/2
	]).

%% FSM exports
-export([start_link/3]).
-export([init/1,
	 code_change/4,
	 handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
	 terminate/3]).

%% Internal exports
-export([receiver/2]).

-include("xmpp.hrl").
-include("exmpp.hrl").

%% No compatibility mode: We use all the nice optimisation of exmpp:
-define(PARSER_OPTIONS,
	[namespace,
	 name_as_atom,
	 ns_check,                        
	 names_check,                                                          
	 attrs_check,                                                          
	 no_maxsize]).

%% API
start() ->
    start_fsm(?defaultserver, ?defaultport, ?defaultserver).

start(Server) ->
    start_fsm(Server, ?defaultport, Server).

start(Server, Port) ->
    start_fsm(Server, Port, Server).

start(Server, Port, Domain) ->
    start_fsm(Server, Port, Domain).

start_fsm(Server, Port, Domain) ->
    gen_fsm:start(?MODULE, [Server, Port, Domain], []).

%% End gen_fsm XMPP process.
stop(Pid) ->
    Pid ! {stop}.

%% Launch connection
%% The connection is a synchronous call
connect(Pid) ->
    gen_fsm:sync_send_event(Pid, {connect}, ?calltimeout).
    
%% Set login information
%% Username, Authentication[, Resource]
%% Authentication is: {password, Password}
set_login_information(Pid, Username, Authentication) ->
    Pid ! {set_login_information, Username, Authentication},
    ok.
set_login_information(Pid, Username, Authentication, Resource) ->
    Pid ! {set_login_information, Username, Authentication, Resource},
    ok.

%% set_host(Pid, Hostname[, Port])
set_host(Pid, Hostname) ->
    Pid ! {set_host, Hostname},
    ok.
set_host(Pid, Hostname, Port) ->
    Pid ! {set_host, Hostname, Port},
    ok.

%% Allow customization of answers to the jabber:iq:version iq packet.
%% In your client code, you can optionnaly call:
%%   xmpp:set_client_info(XMPP, "Client Name ", "1.0"),
%% If not set, default values are provided.
set_client_info(Pid, ClientName, ClientVersion) ->
    Pid ! {set_client_info, ClientName, ClientVersion}.

%% Set the callback module that will received unprocessed XMPP messages
set_callback_module(Pid, Module) ->
    Pid ! {callback_module, Module},
    ok.

%% Send message
message(Pid, To, Type, Subject, Body) ->
%     Message = io_lib:format("<message to='~s' type='~s'><subject>~s</subject><body>~s</body></message>",
% 			    [To, Type, Subject, Body]),
    Message = io_lib:format("<message to='~s' type='~s'><subject>~s</subject><body><![CDATA[~s]]></body></message>",
 			    [To, Type, Subject, Body]),
    Pid ! lists:flatten(Message),
    ok.
message(Pid, To, Type, Body) ->
    Message = io_lib:format("<message to='~s' type='~s'><body>~s</body></message>",[To, Type, Body]),
    Pid ! lists:flatten(Message),
    ok.

%% Send IQ packet and wait for answer
%%  Note: simply use the send function to send iq result, because, we
%%  don't want to wait for answers in this case
iq(Pid, Type, To, Query) ->
    %% Generate a unique reference:
    {A,B,C} = erlang:now(),
    Ref=lists:flatten(io_lib:format("~p~p~p", [A,B,C])),

    IQStanza = case To of
		   [] ->
		       io_lib:format("<iq id='~s' type='~s'>~s</iq>", [Ref,Type,Query]);
		   _ ->
		       io_lib:format("<iq id='~s' to='~s' type='~s'>~s</iq>", [Ref,To,Type,Query])
	       end,
    Pid ! {iq, Ref, self(), lists:flatten(IQStanza)},
    %% Pid ! {iq, Ref, self(), exmpp_xml:iq_to_string(IQStanza)},  EXMPP ?
    receive
	{iqresult, Ref, Result} ->
	    Result
    after 60000 ->
	    {error, timeout}
    end.

%% Function that can be used by a server admin to register a given user:
register_user(Pid, User, Auth) ->
    {password, Password} = Auth,
    Query = io_lib:format("<query xmlns='jabber:iq:register'><username>~s</username><password>~s</password></query>", [User, Password]),
    iq(Pid, "set", "", Query).
   
%% Generic send function (does not wait for answer: Async send)
%% Pid = XMPP reference
send(Pid, XMPP_Packet) ->
    Pid ! lists:flatten(XMPP_Packet).

%% roster_add: Add an item to the users roster
subscribe(Pid, To) ->
    %% Subscribe to user presence:
    PresenceSubscribe = io_lib:format("<presence type='subscribe' to='~s'><status/></presence>",[To]),
    Pid ! lists:flatten(PresenceSubscribe),
    %% Add the user in the roster:
    {A,B,C} = erlang:now(),
    _Ref=lists:flatten(io_lib:format("~p~p~p", [A,B,C])),
    RosterUpdateQuery = io_lib:format("<query xmlns='jabber:iq:roster'><item jid='~s'/></query>", [To]),
    iq(Pid, "set", To, RosterUpdateQuery).

%% FSM callbacks
start_link(Server, Port, Domain) ->
    gen_fsm:start_link(?MODULE, [Server, Port, Domain], []).

init([Server, Port, Domain]) ->
    {ok, unconfigured, #state{host=Server, port=Port, domain=Domain}}.

%% Internal function
open_client_stream(Socket, Host) ->
    case gen_tcp:send(Socket, xml_stream_client(Host)) of
	ok -> ok;
	_Other -> throw(cannot_send_stream)
    end.

%% Get available authentication method for a given user
get_authentication_methods(Socket, Username) ->
    %% Use id attributes to be able to match answer
    Message = io_lib:format("<iq type='get'><query xmlns='jabber:iq:auth'><username>~s</username></query></iq>", [Username]),
    gen_tcp:send(Socket, Message).

%% Send password authentication packet
%% TODO: Make resource configurable...
password_authentication(Socket, Username, Password, Resource) ->
    Message = io_lib:format("<iq type='set'><query xmlns='jabber:iq:auth'><username>~s</username><password>~s</password><resource>~s</resource></query></iq>", [Username, Password, Resource]),
    gen_tcp:send(Socket, Message).

%% TODO: Factorize with low level function generic iq
%% TODO: Handle iq packet id attributes to check that the answer
%%       really correspond to the needed answer
register(Socket, Username, Authentication, Resource) ->
    {password, Password} = Authentication,
    Message = io_lib:format("<iq type='set'><query xmlns='jabber:iq:register'><username>~s</username><password>~s</password><resource>~s</resource></query></iq>", [Username, Password, Resource]),
    gen_tcp:send(Socket, Message).    

send_presence(Socket, Show, Status) ->
    send_presence(Socket, "available", Show, Status).
send_presence(Socket, Type, Show, Status) ->
    Message = io_lib:format("<presence type='~s'><show>~s</show><status>~s</status></presence>", [Type, Show, Status]),
    gen_tcp:send(Socket, Message).

%% Callbacks:
%% We first need to set up mandatory parameters (Login informations)
handle_info({set_login_information, Username, Authentication, Resource}, StateName, StateData) ->
    NewState = case StateName of
		   unconfigured -> ready_to_connect;
		   OtherState   -> OtherState
	       end,
    {next_state, NewState, StateData#state{username = Username,
				   authentication = Authentication,
				   resource = Resource}};
handle_info({set_login_information, Username, Authentication}, StateName, StateData) ->
    NewState = case StateName of
		   unconfigured -> ready_to_connect;
		   OtherState   -> OtherState
	       end,
    {next_state, NewState, StateData#state{username = Username,
				   authentication = Authentication}};
%% Set host[, Port]
handle_info({set_host, Hostname}, StateName, StateData) ->
    {next_state, StateName, StateData#state{host = Hostname}};
handle_info({set_host, Hostname, Port}, StateName, StateData) ->
    {next_state, StateName, StateData#state{host = Hostname, port = Port}};
%% Client_info (jabber:iq:version) customization)
handle_info({set_client_info, ClientName, ClientVersion}, StateName, StateData) ->
    {next_state, StateName, StateData#state{client_name = ClientName,
					    client_version = ClientVersion}};
%% The first callbacks are used as an API in all states
%% Set the callback module that will "receive" unprocessed messages
handle_info({callback_module, Module}, StateName, StateData) ->
    %% Changed callback module
    {next_state, StateName, StateData#state{callback_module=Module}};
%% Stop the XMPP gen_fsm
handle_info({stop}, _StateName, StateData) ->
    %% Disconnecting from the XMPP server
    gen_tcp:send(StateData#state.socket, ?STREAM_TRAILER),
    gen_tcp:close(StateData#state.socket),

    %% Necessary to close the xml_stream parser:
    %xml_stream:close(StateData#state.xml_stream),
    exmpp_xmlstream:stop(StateData#state.xml_stream), % EXMPP

    {stop, normal, StateData};
%% IQ: Store the tuple {IQIDRef, CallerPid} in the StateData
handle_info({iq, Ref, CallerPid, Stanza}, StateName, StateData) ->
    %% ?INFO_MSG("Sending [~p]", [Stanza]),
    IQRefList = StateData#state.iq_ref_list,
    NewStateData = StateData#state{iq_ref_list=IQRefList++[{Ref,CallerPid}]},
    gen_tcp:send(StateData#state.socket, Stanza),    
    {next_state, StateName, NewStateData};
%% Use handle info to send data from the client to the server
%% (bidirectional)
%% Define the data that we can send in a wrapper API
handle_info(Message, StateName, StateData) ->
    %% ?INFO_MSG("Sending [~p]", [Message]),
    gen_tcp:send(StateData#state.socket, Message),
    {next_state, StateName, StateData}.

%% Synchrone calls to connect.
unconfigured({connect}, _From, StateData) ->
    %% Mandatory parameters not set (Username, Authentication)
    {reply, {error, unconfigured}, unconfigured, StateData#state{from_pid=undefined}}.
ready_to_connect({connect}, From, StateData) ->
    Host = StateData#state.host,
    Port = StateData#state.port,
    Domain = StateData#state.domain,
    Socket = case gen_tcp:connect(Host, Port, [{packet,0},
					       binary,
					       {active, false},
					       {reuseaddr, true}], infinity) of
		 {ok, Sock}      -> 
		     ?INFO_MSG("Connected to ~s:~p~n", [Host,Port]),
		     Sock;
		 {error, Reason} -> ?ERROR_MSG("Connection error [~p]~n",
					       [Reason]),
				    exit(Reason)
	     end,

    open_client_stream(Socket, Domain),

    Stream = case exmpp_xmlstream:start({gen_fsm, self()}, ?PARSER_OPTIONS) of
		 {ok, S} -> S;
		 {error, XmlReason} -> throw(XmlReason)
	     end,
    _ReceiverPid = spawn(?MODULE, receiver, [Socket, Stream]),

    {next_state, wait_for_stream, StateData#state{socket = Socket, xml_stream = Stream, from_pid=From}}.

wait_for_stream(#xmlstreamstart{element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=stream}}, StateData) ->
    %% The stream is now open
    %% Retrieve supported authentication methods:
    get_authentication_methods(StateData#state.socket, StateData#state.username),
    {next_state, wait_for_authentication_method, StateData};
%% Normally a stream error cannot happen here.
%% The server should first return an opening stream element before sending
%% the error.
wait_for_stream(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error}=StreamError}, StateData) ->
    Reason = stream_error(StreamError),
    {stop, {error, Reason}, StateData}.

%% Get authentication methods to choose one
wait_for_authentication_method(#xmlstreamelement
			       {element=#xmlnselement{name=iq}=IQElement},
			       StateData) ->
    %% Extracting the auth methods:
    QueryElement = exmpp_xml:get_element_by_name(IQElement, 'jabber:iq:auth', 'query'),
    
    %% Use password authentication first
    %% TODO: support digest and resource
    case exmpp_xml:get_element_by_name(QueryElement, 'jabber:iq:auth', 'password') of
	false -> 
	    %% Jabberlang library only support password authentication for now.
	    %% If Password authentication is not accepted by the server
	    %% we stop there:
	    {stop, {error, authentication_method_not_supported}, StateData};
	_ ->
	    %% Authentication
	    {password, Password} = StateData#state.authentication,
	    password_authentication(StateData#state.socket,
				    StateData#state.username,
				    Password,
				    StateData#state.resource),
	    {next_state, wait_for_authentication_result, StateData}
    end;
%% Error: Disconnected
wait_for_authentication_method(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error,
				  children=[#xmlcdata{cdata=  <<"Disconnected">> }]}},
			       StateData) ->
    gen_fsm:reply(StateData#state.from_pid, {error, disconnected}),
    {stop, {error, disconnected}, StateData};
%% Error in the stream
wait_for_authentication_method(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error}=StreamError}, StateData) ->
    Reason = stream_error(StreamError),
    {stop, {error, Reason}, StateData}.

%% Authentication successfull
wait_for_authentication_result(#xmlstreamelement
			       {element=
				#xmlnselement{name=iq,
					      attrs=[#xmlattr{name=type,
							      value="result"}]}},
			       StateData) ->
    ?INFO_MSG("Authentication successfull. You are logged in as ~p ~n", [StateData#state.username]),
    %% After authentication, send presence information (TODO: Move that after roster retrieval)
    send_presence(StateData#state.socket, StateData#state.show, StateData#state.status),
    gen_fsm:reply(StateData#state.from_pid, ok),
    {next_state, wait_for_element, StateData#state{from_pid=undefined}};
%% Error: Disconnected
wait_for_authentication_result(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error,
				  children=[#xmlcdata{cdata=  <<"Disconnected">> }]}},
			       StateData) ->
    {stop, {error, disconnected}, StateData};
%% If unauthorized: Try to register user...
%% Stanza error
wait_for_authentication_result(#xmlstreamelement
			       {element=
				#xmlnselement{name=iq,
					      attrs=[#xmlattr{name=type,
							      value="error"}]}=IQElement},
			       StateData) ->
    %% Extract stanza error codes:
    {_Code, _Type, _Reason} = stanza_error(IQElement),
    case StateData#state.auto_registration of
	false ->
	    gen_fsm:reply(StateData#state.from_pid, {error, authentication_failed}),
	    {stop, {error,authentication_failed}, StateData#state{from_pid=undefined}};
	true ->
	    %% Authentication failed
	    %% Trying to register user
	    register(StateData#state.socket, StateData#state.username,
			  StateData#state.authentication, StateData#state.resource),
	    {next_state, wait_for_registration_result, StateData}
    end.

%% MREMOND: TODO
%% Remove auto registation from the FSM.
%% It should be move to the API and explicitely called by the developer.
wait_for_registration_result(#xmlstreamelement
			       {element=#xmlnselement{name=iq,
						      attrs=Attrs}},
			     StateData) ->
    %% Registration failed
    case exmpp_xml:get_attribute_node_from_list(Attrs, type) of
	%% Registration successfull:
	#xmlattr{value="result"} ->
	    %% Successfully registered user
	    %% After registration, we need to redo authentication
  	    {password, Password} = StateData#state.authentication,
  	    password_authentication(StateData#state.socket,
  				    StateData#state.username,
  				    Password,
  				    StateData#state.resource),
	    gen_fsm:reply(StateData#state.from_pid, ok),
  	    {next_state, wait_for_authentication_result, StateData#state{auto_registration=false}};
	_ ->
	    gen_fsm:reply(StateData#state.from_pid, {error, registration_failed}),
	    {stop, {error,registration_failed}, StateData#state{from_pid=undefined}}
    end;
%% Error: Disconnected
wait_for_registration_result(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error,
				  children=[#xmlcdata{cdata=  <<"Disconnected">> }]}},
			       StateData) ->
    {stop, {error, disconnected}, StateData};
%% Stream Error
wait_for_registration_result(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error}=StreamError}, StateData) ->
    Reason = stream_error(StreamError),
    {stop, {error, Reason}, StateData}.

wait_for_element(#xmlstreamelement
			       {element=#xmlnselement{
				  ns='http://etherx.jabber.org/streams',
				  name=error}=StreamError}, StateData) ->
    Reason = stream_error(StreamError),
    {stop, {error, Reason}, StateData};
%% TODO: Check End Element.
wait_for_element(#xmlstreamend{}, StateData) ->
    {stop, normal, StateData};
%% Process Message packet:
wait_for_element(#xmlstreamelement
        {element=#xmlnselement{name=message, attrs=Attrs}=Msg}, StateData) ->
    %% Set default type
    case exmpp_xml:get_attribute_node_from_list(Attrs, type) of
	false ->
	    process_message(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      "normal", Attrs, Msg);
	#xmlattr{value=Type} ->
	    process_message(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      Type, Attrs, Msg)
    end,
    {next_state, wait_for_element, StateData};
%% presence packet:
wait_for_element(#xmlstreamelement
		 {element=#xmlnselement{name=presence, attrs=Attrs}=Pres}, StateData) ->
    case exmpp_xml:get_attribute_node_from_list(Attrs, type) of
	false -> process_presence(
		   self(),
		   StateData#state.socket,
		   StateData#state.callback_module,
		   "available", Attrs, Pres);
	#xmlattr{value=Type} ->
	    process_presence(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      Type, Attrs, Pres)
    end,
    {next_state, wait_for_element, StateData};
%% Process IQ element: If this is a result: send the data back to the
%% waiting process. Otherwise use callback to pass it to the standard callback module.
wait_for_element(#xmlstreamelement
		 {element=#xmlnselement{name=iq, attrs=Attrs}=IQ}, StateData) ->
    %%If this is a result: send the data back to the waiting process
    NewStateData = 
	case exmpp_xml:get_attribute_node_from_list(Attrs, type) of
	    #xmlattr{value="result"} ->
		case exmpp_xml:get_attribute_node_from_list(Attrs, id) of
		    false -> 
			%% Error: IQ result without id attribute
			StateData;
		    #xmlattr{value=Ref} ->
			NewIQRefList = process_iq_result(StateData#state.iq_ref_list,
							 Ref,
							 IQ),
			StateData#state{iq_ref_list=NewIQRefList}
		end;
	    #xmlattr{value="error"} ->
		case exmpp_xml:get_attribute_node_from_list(Attrs, id) of
		    false -> 
			%% Error: IQ result without id attribute
			StateData;
		    #xmlattr{value=Ref} ->
			NewIQRefList = process_iq_result(StateData#state.iq_ref_list,
							 Ref,
							 IQ),
			StateData#state{iq_ref_list=NewIQRefList}
		end;
	    #xmlattr{value=Type} ->
		process_iq(
		  self(),
		  StateData#state.socket,
		  StateData#state.callback_module,
		  Type, Attrs, IQ,
		  StateData),
		StateData
	end,
    {next_state, wait_for_element, NewStateData};
wait_for_element(_XML, StateData) ->
    {next_state, wait_for_element, StateData}.

%% Someone is asking for presence subscription
%% By default, enable subscription and add this people to the roster
%% TODO: Use a parameter to determine if autosubscribtion is allowed
%% TODO: see status element content for subscribtion reason:
%% 		   [{xmlelement,
%% 		     "status",
%% 		     [],
%% 		     [{xmlcdata,
%% 		       "I would like to add you to my roster."},
%% 		      {xmlcdata,"\n"}]}]}}
%% TODO: We should make that easy to handle with the API (and we should propose a
%%   new client template, that setup this default behaviour)
process_presence(_Pid, Socket, _Module, "subscribe", Attrs, _Pres) ->
    #xmlattr{value=Who} = exmpp_xml:get_attribute_node_from_list(Attrs, from),
    PresenceTag = "<presence type='~s' to='~s'/>",
    Allowsubscribtion = io_lib:format(PresenceTag,["subscribed", Who]),
    gen_tcp:send(Socket, Allowsubscribtion),
    %% Todo avoid infinite loop
    Subscribe = io_lib:format(PresenceTag,["subscribe", Who]),
    gen_tcp:send(Socket, Subscribe);
process_presence(_Pid, _Socket, _Module, "subscribed", Attrs, _Pres) ->
    #xmlattr{value=Who} = exmpp_xml:get_attribute_node_from_list(Attrs, from),    
    ?INFO_MSG("Now subscribed to ~s", [Who]);
%% Generic presence: use callbacks
%% TODO: For available presence: Extract show, status (and priority ?)
process_presence(Pid, _Socket, Module, Type, Attrs, Pres) -> 
    #xmlattr{value=Who} = exmpp_xml:get_attribute_node_from_list(Attrs, from),    
    Module:presence(Pid, Type, Who, Attrs, Pres).

%% The client has received a message
%% Use callback module to "send" it to the client
process_message(Pid, _Socket, Module, Type, Attrs, Msg) ->
    #xmlattr{value=Who} = exmpp_xml:get_attribute_node_from_list(Attrs, from),
    Body = exmpp_xml:get_cdata(exmpp_xml:get_element_by_name(Msg, body)),
    Subject = exmpp_xml:get_cdata(exmpp_xml:get_element_by_name(Msg, subject)),
    Module:message(Pid, Type, Who, Subject, Body, Attrs, Msg).

%% The client has received an iq query
process_iq(Pid, _Socket, Module, Type, Attrs, IQ, StateData) ->
    case exmpp_xml:get_attribute_node_from_list(Attrs, id) of
	false ->
	    %% IQ query without id. Ignoring
	    ignore;
	#xmlattr{value=Ref} ->
	    #xmlattr{value=Who} = exmpp_xml:get_attribute_node_from_list(Attrs, from),
	    %% Extract query namespace
	    QueryNS = case exmpp_xml:get_element_by_name(IQ, 'query') of
			  false -> undefined;
			  QueryElement -> 
			      false_to_undefined(exmpp_xml:get_attribute_node(QueryElement,
									      xmlns))
		      end,
	    automatic_iq(Pid, Module, Type, Who, QueryNS, Ref, Attrs, IQ, StateData)
    end.

%% Send the IQ result to the waiting process
%% TODO: Get Namespace and process some of them from the generic
%%       client module
process_iq_result(IQRefList, Ref, Stanza) ->
    case lists:keysearch(Ref,1,IQRefList) of
	{value, {Ref, Pid}} ->
	    %% Received IQ answer
	    Pid ! {iqresult, Ref, Stanza},
	    _NewIQRefList = lists:keydelete(Ref, 1, IQRefList);
	_Other ->
	    %% IQ result without existing id reference: ignore
	    IQRefList
    end.

%% Convert false to undefined or return other values unchanged.
false_to_undefined(false) -> undefined;
false_to_undefined(Other) -> Other.

%% Wrapper pour les requêtes iq synchrone vers un équivalent de call
%% en Erlang.  Introduire un mechanisme pour redispatcher la réponse
%% vers le bon process qui peut se mettre en attente. Le process qui
%% récupère le mode d'authentification se met en attente sur un
%% receive {iq, result, Id, Content}, avec l'Id attribué. Il déclare
%% aussi le mapping entre Id et PId à un process dispatcheur.. Lorsque
%% le process de réception (receiver) récupérer un iq type result, il
%% récupère l'id, à partir de là retrouve le pid du process qui a émis
%% l'id, et renvoie la réponse à ce processus, qui récupère la réponse
%% et se trouve ainsi débloqué. Prévoir un timeout pour le processus
%% en attente, (mais assez long).

receiver(Socket, Stream) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    {ok, NewStream} = exmpp_xmlstream:parse(Stream, Data),
	    receiver(Socket, NewStream);
	{error, _Reason} -> 
	    ok %% End receiver TODO: End other process
    end.


%% Formatting Jabber XML
xml_stream_client(Server) ->
    io_lib:format(?STREAM_CLIENT_HEADER, [Server]).

%% Automatic handler for jabber:iq:version queries
automatic_iq(Pid, _Module, _Type, From, "jabber:iq:version", PacketID, _Attrs, _SubElts, StateData) ->
    ClientName    = StateData#state.client_name,
    ClientVersion = StateData#state.client_version,
    %% Format IQ result:
    XMPP_Packet = xmpp_iq:version_r(PacketID, From, ClientName, ClientVersion),
    %% The the resulting packet (async).
    xmpp:send(Pid, XMPP_Packet);

%% No automatic IQ reply: Let the callback module handle the answer.
automatic_iq(Pid, Module, Type, From, QueryNS, PacketID, Attrs, IQ, _StateData) ->
    Module:iq(Pid, Type, From, QueryNS, PacketID, Attrs, IQ).

%% OTP related code
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(normal, _StateName, _StateData) ->
    ok;
%% Gracefull shutdown: Reply to process waiting for a reply
terminate({error, Reason}, _StateName, StateData) ->
    gen_fsm:reply(StateData#state.from_pid, {error, Reason}).

handle_event(Event, StateName, StateData) ->
    io:format("handle_event event: ~p ~n", [Event]),
    {next_state, StateName, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("handle_sync_event event: ~p ~n", [Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% Extract and return error code from XMPP stream:error:
stream_error(StreamError) ->
    case exmpp_xml:get_element_by_ns(StreamError,
				     'urn:ietf:params:xml:ns:xmpp-streams') of
	false -> undefined;
	#xmlnselement{name=Name} -> Name;
	_Other -> undefined
    end.

%% Extract error information from XMPP stanza error:
stanza_error(IQError) ->
    ErrorElement = exmpp_xml:get_element_by_name(IQError, 'jabber:client', error),
    Code = exmpp_xml:get_attribute_node(ErrorElement, code),
    Type = exmpp_xml:get_attribute_node(ErrorElement, type),
    ReasonElement = exmpp_xml:get_element_by_ns(ErrorElement,
						'urn:ietf:params:xml:ns:xmpp-stanzas'),
    {Code#xmlattr.value, Type#xmlattr.value, ReasonElement#xmlnselement.name}.

%% Récupération de statistiques
% OUT(1,mremond@localhost/tkabber):
% <iq id='12'
% 	to='localhost'
% 	type='get'
% 	xml:lang='fr'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/online'/>
%   </query>
% </iq>
% IN(1,mremond@localhost/tkabber):
% <iq from='localhost'
% 	to='mremond@localhost/tkabber'
% 	id='12'
% 	type='result'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/online'
% 	units='users'
% 	value='1'/>
%   </query>
% </iq>
% OUT(1,mremond@localhost/tkabber):
% <iq id='13'
% 	to='localhost'
% 	type='get'
% 	xml:lang='fr'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/total'/>
%   </query>
% </iq>
% IN(1,mremond@localhost/tkabber):
% <iq from='localhost'
% 	to='mremond@localhost/tkabber'
% 	id='13'
% 	type='result'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/total'
% 	units='users'
% 	value='1'/>
%   </query>
% </iq>
