% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle a
%% TLS session.

-module(exmpp_tls).
-vsn('$Revision$').

-behaviour(gen_server).

% Initialization.
-export([
  start/0,
  start_link/0
]).

% Registry handling.
-export([
  register_engine/3,
  register_engine/4,
  get_auth_methods/0,
  get_engine_names/0,
  get_engine_names/1,
  get_prefered_engine_name/1,
  get_engine_driver/1
]).

% Handshake.
-export([
  connect/4,
  accept/4
]).

% Common socket API.
-export([
  send/2,
  recv/2,
  recv/3,
  controlling_process/2,
  close/1,
  port_revision/1
]).

% Communication with the underlying module/socket.
-export([
  underlying_send/2,
  underlying_recv/2
]).

% gen_server(3erl) callbacks.
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  engines,
  by_auth_method
}).

-record(tls_engine, {
  name,
  driver_path,
  driver,
  auth_methods = []
}).

-record(tls_socket, {
  socket,
  packet_mode = binary,
  port
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_ENGINE, openssl).

-define(COMMAND_SET_MODE,              1).
-define(COMMAND_SET_IDENTITY,          2).
-define(COMMAND_SET_PEER_VERIF,        3).
-define(COMMAND_SET_TRUSTED_CERTS,     4).
-define(COMMAND_SET_OPTIONS,           5).
-define(COMMAND_PREPARE_HANDSHAKE,     6).
-define(COMMAND_HANDSHAKE,             7).
-define(COMMAND_SET_ENCRYPTED_INPUT,   8).
-define(COMMAND_GET_DECRYPTED_INPUT,   9).
-define(COMMAND_SET_DECRYPTED_OUTPUT, 10).
-define(COMMAND_GET_ENCRYPTED_OUTPUT, 11).
-define(COMMAND_SHUTDOWN,             12).
-define(COMMAND_SVN_REVISION,         13).

% --------------------------------------------------------------------
% Initialization.
% --------------------------------------------------------------------

%% @hidden

start() ->
    Ret = gen_server:start({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.

%% @hidden

start_link() ->
    Ret = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.

-ifdef(HAVE_OPENSSL).
-define(REGISTER_OPENSSL,
  register_engine(openssl, exmpp_tls_openssl, [{x509, 10}])).
-else.
-define(REGISTER_OPENSSL, ok).
-endif.

-ifdef(HAVE_GNUTLS).
-define(REGISTER_GNUTLS,
  register_engine(gnutls, exmpp_tls_gnutls, [{x509, 20}, {openpgp, 10}])).
-else.
-define(REGISTER_GNUTLS, ok).
-endif.

register_builtin_engines() ->
    ?REGISTER_OPENSSL,
    ?REGISTER_GNUTLS,
    ok.

% --------------------------------------------------------------------
% Registry handling.
% --------------------------------------------------------------------

%% @spec (Name, Driver, Auth_Methods) -> ok
%%     Name = atom()
%%     Driver = atom()
%%     Auth_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new TLS engine.

register_engine(Name, Driver, Auth_Methods) ->
    register_engine(Name, undefined, Driver, Auth_Methods).

%% @spec (Name, Driver_Path, Driver, Auth_Methods) -> ok
%%     Name = atom()
%%     Driver_Path = string()
%%     Driver = atom()
%%     Auth_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new TLS engine.

register_engine(Name, Driver_Path, Driver, Auth_Methods)
  when is_atom(Name), is_list(Auth_Methods), length(Auth_Methods) > 0 ->
    Engine = #tls_engine{
      name = Name,
      driver_path = Driver_Path,
      driver = Driver,
      auth_methods = Auth_Methods
    },
    gen_server:cast(?SERVER, {register_engine, Engine}).

%% @spec () -> [Auth_Method]
%%     Auth_Method = atom()
%% @doc Return the list of supported auth methods.

get_auth_methods() ->
    gen_server:call(?SERVER, get_auth_methods).

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of TLS engines.

get_engine_names() ->
    gen_server:call(?SERVER, get_engine_names).

%% @spec (Auth_Method) -> [Engine_Name]
%%     Auth_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the list of TLS engines which support the given auth method.
%%
%% The list is sorted from the most to the least prefered engine.

get_engine_names(Auth_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Auth_Method}),
    [E#tls_engine.name || E <- Engines].

%% @spec (Auth_Method) -> [Engine_Name]
%%     Auth_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the name of the prefered TLS engines which support the
%% given auth method.

get_prefered_engine_name(Auth_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Auth_Method}),
    case Engines of
        []           -> undefined;
        [Engine | _] -> Engine
    end.

%% @spec (Engine_Name) -> Driver
%%     Engine_Name = atom()
%%     Driver = Driver_Name | {Driver_Path, Driver_Name}
%%     Driver_Name = atom()
%%     Driver_Path = string()
%% @doc Return the port driver name associated to the given engine.

get_engine_driver(Engine_Name) ->
    case gen_server:call(?SERVER, {get_engine, Engine_Name}) of
        undefined ->
            undefined;
        #tls_engine{driver_path = undefined, driver = Driver_Name} ->
            Driver_Name;
        #tls_engine{driver_path = Driver_Path, driver = Driver_Name} ->
            {Driver_Path, Driver_Name}
    end.

% --------------------------------------------------------------------
% Handshake.
% --------------------------------------------------------------------

%% @spec (Socket_Desc, Identity, Peer_Verification, Options) -> TLS_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Identity = {Auth_Method, Certificate, Private_Key} | undefined
%%     Auth_Method = atom()
%%     Certificate = string()
%%     Private_Key = string()
%%     Peer_Verification = boolean() | Peer_Name
%%     Peer_Name = string()
%%     Options = [Option]
%%     Option = {engine, Engine} | {mode, Mode} | {trusted_certs, Auth_Method, Certs} | peer_cert_required | accept_expired_cert | accept_revoked_cert | accept_non_trusted_cert | accept_corrupted_cert
%%     Engine = atom()
%%     Mode = binary | list
%%     TLS_Socket = tls_socket()
%% @doc Start TLS handshake as a client.

connect(Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(client, Socket_Desc, Identity, Peer_Verification, Options).

%% @spec (Socket_Desc, Identity, Peer_Verification, Options) -> TLS_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Identity = {Auth_Method, Certificate, Private_Key}
%%     Auth_Method = atom()
%%     Certificate = string()
%%     Private_Key = string()
%%     Peer_Verification = boolean() | Peer_Name
%%     Peer_Name = string()
%%     Options = [Option]
%%     Option = {engine, Engine} | {mode, Mode} | {trusted_certs, Auth_Method, Certs} | peer_cert_required | accept_expired_cert | accept_revoked_cert | accept_non_trusted_cert | accept_corrupted_cert
%%     Engine = atom()
%%     Mode = binary | list
%%     TLS_Socket = tls_socket()
%% @doc Start TLS handshake as a server.

accept(Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(server, Socket_Desc, Identity, Peer_Verification, Options).

handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options,
      infinity).

handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options,
  Recv_Timeout) ->
    % Check arguments.
    check_identity(Identity, Mode),
    check_peer_verification(Peer_Verification, Mode),

    % Start a port driver instance.
    Driver_Name = get_engine_from_args(Identity, Peer_Verification, Options),
    Port = try
        exmpp_internals:open_port(Driver_Name)
    catch
        Exception1 ->
            exmpp_internals:unload_driver(Driver_Name),
            throw(Exception1)
    end,

    % Initialize the port and handshake.
    try
        % Set mode (client vs. server).
        engine_set_mode(Port, Mode),

        % Set local peer identity.
        engine_set_identity(Port, Identity),

        % Enable (or not) peer's certificate verification.
        engine_set_peer_verification(Port, Peer_Verification),

        % XXX Set options.

        % Handshake!
        engine_prepare_handshake(Port),
        handshake2(Mode, Socket_Desc, Port, Recv_Timeout)
    catch
        Exception2 ->
            exmpp_internals:close_port(Port),
            exmpp_internals:unload_driver(Driver_Name),
            throw(Exception2)
    end.

handshake2(client = Mode, Socket_Desc, Port, Recv_Timeout) ->
    % Try to handshake.
    case engine_handshake(Port) of
        want_read ->
            % Send the current data.
            New_Packet = engine_get_encrypted_output(Port),
            case underlying_send(Socket_Desc, New_Packet) of
                ok ->
                    % Wait for a packet from the client.
                    case underlying_recv(Socket_Desc, Recv_Timeout) of
                        {ok, Packet} ->
                            engine_set_encrypted_input(Port, Packet),
                            % Recurse!
                            handshake2(Mode, Socket_Desc, Port, Recv_Timeout);
                        {error, Reason} ->
                            throw({tls, handshake, underlying_recv, Reason})
                    end;
                {error, Reason} ->
                    throw({tls, handshake, underlying_send, Reason})
            end;
        ok ->
            % Handshake done.
            #tls_socket{socket = Socket_Desc, port = Port}
    end;
handshake2(server = Mode, Socket_Desc, Port, Recv_Timeout) ->
    % Wait for a packet from the client.
    case underlying_recv(Socket_Desc, Recv_Timeout) of
        {ok, Packet} ->
            engine_set_encrypted_input(Port, Packet),
            % Try to handshake.
            case engine_handshake(Port) of
                want_read ->
                    % Send the current data.
                    New_Packet = engine_get_encrypted_output(Port),
                    case underlying_send(Socket_Desc, New_Packet) of
                        ok ->
                            % Recurse!
                            handshake2(Mode, Socket_Desc, Port, Recv_Timeout);
                        {error, Reason} ->
                            throw({tls, handshake, underlying_send, Reason})
                    end;
                ok ->
                    New_Packet = engine_get_encrypted_output(Port),
                    case underlying_send(Socket_Desc, New_Packet) of
                        ok ->
                            % Handshake done.
                            #tls_socket{socket = Socket_Desc, port = Port};
                        {error, Reason} ->
                            throw({tls, handshake, underlying_send, Reason})
                    end
            end;
        {error, Reason} ->
            throw({tls, handshake, underlying_recv, Reason})
    end.

% --------------------------------------------------------------------
% Handshake helpers.
% --------------------------------------------------------------------

get_engine_from_args(Identity, Peer_Verification, Options) ->
    Engine_Name = case get_engine_from_options(Options) of
        undefined ->
            case get_engine_from_identity(Identity) of
                undefined ->
                    case get_engine_from_verification(Peer_Verification) of
                        undefined ->
                            ?DEFAULT_ENGINE;
                        Name ->
                            Name
                    end;
                Name ->
                    Name
            end;
        Name ->
            Name
    end,
    case get_engine_driver(Engine_Name) of
        {Driver_Path, Driver_Name} ->
            exmpp_internals:load_driver(Driver_Name, [Driver_Path]),
            Driver_Name;
        Driver_Name ->
            exmpp_internals:load_driver(Driver_Name),
            Driver_Name
    end.

get_engine_from_options(Options) ->
    case lists:keysearch(engine, 1, Options) of
        {value, {_, Engine_Name}} -> Engine_Name;
        _                         -> undefined
    end.

get_engine_from_identity(Identity) ->
    case lists:keysearch(certificate, 1, Identity) of
        {value, {_, Auth_Method, _}} ->
            get_prefered_engine_name(Auth_Method);
        _ ->
            undefined
    end.

get_engine_from_verification(Peer_Verification) ->
    case lists:keysearch(trusted_certs, 1, Peer_Verification) of
        {value, {_, Auth_Method, _}} ->
            get_prefered_engine_name(Auth_Method);
        _ ->
            undefined
    end.

check_identity(Identity, Mode) ->
    case Identity of
        {AM, Cert, PK} when is_atom(AM), is_list(Cert), is_list(PK) ->
            case io_lib:deep_char_list(Cert) of
                false -> throw({tls, handshake, invalid_certificate, Cert});
                _     -> ok
            end,
            case io_lib:deep_char_list(PK) of
                false -> throw({tls, handshake, invalid_private_key, PK});
                _     -> ok
            end;
        undefined when Mode == client ->
            ok;
        undefined when Mode == server ->
            throw({tls, handshake, identity_mandatory, Identity});
        _ ->
            throw({tls, handshake, invalid_identity, Identity})
    end.

check_peer_verification(Peer_Verif, _Mode) ->
    case Peer_Verif of
        true ->
            ok;
        false ->
            ok;
        E when is_list(E) ->
            case io_lib:deep_char_list(E) of
                false -> throw({tls, handshake, invalid_peer_verification, E});
                _     -> ok
            end;
        _ ->
            throw({tls, handshake, invalid_peer_verification, Peer_Verif})
    end.

% --------------------------------------------------------------------
% Common socket API.
% --------------------------------------------------------------------

%% @spec (TLS_Socket, Packet) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Packet = binary() | list()
%%     Reason = term()
%% @doc Send `Packet' over a TLS-protected connection.

send(#tls_socket{socket = Socket_Desc, port = Port}, Packet) ->
    try
        engine_set_decrypted_output(Port, Packet),
        Encrypted = engine_get_encrypted_output(Port),
        underlying_send(Socket_Desc, Encrypted)
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (TLS_Socket, Length) -> {ok, Packet} | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Length = integer()
%%     Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a TLS-protected connection.

recv(Socket_Data, Length) ->
    recv(Socket_Data, Length, infinity).

%% @spec (TLS_Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Length = integer()
%%     Timeout = integer()
%%     Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a TLS-protected connection.

recv(Socket_Data, Length, Timeout) ->
    recv2(Socket_Data, Length, Timeout, <<>>).

recv2(#tls_socket{packet_mode = Mode}, Length, _Timeout, Previous_Data)
  when size(Previous_Data) > 0, Length =< 0 ->
      case Mode of
          binary -> {ok, Previous_Data};
          list   -> {ok, binary_to_list(Previous_Data)}
      end;
recv2(#tls_socket{socket = Socket_Desc, port = Port} = Socket_Data,
  Length, Timeout, Previous_Data) ->
    try
        case engine_get_decrypted_input(Port, Length) of
            want_read ->
                % Ok, we need more data.
                {Recv, New_Timeout} = case Timeout of
                    infinity ->
                        {underlying_recv(Socket_Desc, Timeout), Timeout};
                    _ ->
                        {Elapsed, Ret} = timer:tc(?MODULE, underlying_recv,
                          [Socket_Desc, Timeout]),
                        {Ret, Timeout - Elapsed div 1000}
                end,
                case Recv of
                    {ok, Packet} ->
                        engine_set_encrypted_input(Port, Packet),
                        % Try to decipher it.
                        recv2(Socket_Data, Length, New_Timeout,
                          Previous_Data);
                    {error, Reason} ->
                        {error, Reason}
                end;
            Data ->
                % Got a chunk of plain-text data.
                Ack = engine_get_encrypted_output(Port),
                case underlying_send(Socket_Desc, Ack) of
                    ok ->
                        recv2(Socket_Data,
                          Length - size(Data), Timeout,
                          <<Previous_Data/binary, Data/binary>>);
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (TLS_Socket, Pid) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Pid = pid()
%%     Reason = term()
%% @doc Change the controlling socket of the underlying socket.

controlling_process(#tls_socket{socket = {Mod, Socket}}, Pid) ->
    Mod:controlling_process(Socket, Pid).

%% @spec (TLS_Socket) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Reason = term()
%% @doc Shutdown the TLS session and close the underlying socket.

close(#tls_socket{socket = {Mod, Socket} = Socket_Data,
  port = Port}) ->
    % First, shutdown the TLS session.
    engine_shutdown(Port),
    Notify = engine_get_encrypted_output(Port),
    case underlying_send(Socket_Data, Notify) of
        ok ->
            % Close the underlying socket.
            Mod:close(Socket);
        {error, Reason} ->
            {error, Reason}
    end.

%% @hidden

port_revision(#tls_socket{port = Port}) ->
    engine_svn_revision(Port).

% --------------------------------------------------------------------
% Engine function wrappers.
% --------------------------------------------------------------------

control(Port, Command, Data) ->
    case port_control(Port, Command, Data) of
        <<0, Result/binary>> -> Result;
        <<1, Error/binary>>  -> {error, binary_to_term(Error)};
        <<2>>                -> want_read;
        <<3>>                -> want_write
    end.

engine_set_mode(Port, Mode) ->
    Mode_ID = case Mode of
        server -> 1;
        client -> 2
    end,
    case control(Port, ?COMMAND_SET_MODE, term_to_binary(Mode_ID)) of
        {error, Reason} ->
            throw({tls, handshake, set_mode_failure, Reason});
        _ ->
            ok
    end.

engine_set_identity(_Port, undefined) ->
    ok;
engine_set_identity(Port, Identity) ->
    case control(Port, ?COMMAND_SET_IDENTITY, term_to_binary(Identity)) of
        {error, Reason} ->
            throw({tls, handshake, set_identity_failure, Reason});
        _ ->
            ok
    end.

engine_set_peer_verification(Port, Peer_Verif) ->
    case control(Port, ?COMMAND_SET_PEER_VERIF, term_to_binary(Peer_Verif)) of
        {error, Reason} ->
            throw({tls, handshake, set_peer_verification, Reason});
        _ ->
            ok
    end.

engine_prepare_handshake(Port) ->
    case control(Port, ?COMMAND_PREPARE_HANDSHAKE, <<>>) of
        {error, Reason} ->
            throw({tls, handshake, prepare_handshake, Reason});
        _ ->
            ok
    end.

engine_handshake(Port) ->
    case control(Port, ?COMMAND_HANDSHAKE, <<>>) of
        {error, Reason} ->
            throw({tls, handshake, do_handshake, Reason});
        <<>> ->
            ok;
        Result ->
            Result
    end.

engine_set_encrypted_input(Port, Data) when is_list(Data) ->
    engine_set_encrypted_input(Port, list_to_binary(Data));
engine_set_encrypted_input(Port, Data) ->
    case control(Port, ?COMMAND_SET_ENCRYPTED_INPUT, Data) of
        {error, Reason} ->
            throw({tls, recv, set_encrypted_input, Reason});
        _ ->
            ok
    end.

engine_get_decrypted_input(Port, Length) ->
    case control(Port, ?COMMAND_GET_DECRYPTED_INPUT, term_to_binary(Length)) of
        {error, Reason} ->
            throw({tls, recv, get_decrypted_input, Reason});
        Result ->
            Result
    end.

engine_set_decrypted_output(Port, Data) when is_list(Data) ->
    engine_set_decrypted_output(Port, list_to_binary(Data));
engine_set_decrypted_output(Port, Data) ->
    case control(Port, ?COMMAND_SET_DECRYPTED_OUTPUT, Data) of
        {error, Reason} ->
            throw({tls, send, set_decrypted_output, Reason});
        _ ->
            ok
    end.

engine_get_encrypted_output(Port) ->
    case control(Port, ?COMMAND_GET_ENCRYPTED_OUTPUT, <<>>) of
        {error, Reason} ->
            throw({tls, send, get_encrypted_output, Reason});
        Result ->
            Result
    end.

engine_svn_revision(Port) ->
    case control(Port, ?COMMAND_SVN_REVISION, <<>>) of
        {error, Reason} ->
            throw({tls, handshake, svn_revision, Reason});
        Revision ->
            binary_to_term(Revision)
    end.

engine_shutdown(Port) ->
    case control(Port, ?COMMAND_SHUTDOWN, <<>>) of
        {error, Reason} ->
            throw({tls, shutdown, shutdown_failed, Reason});
        Result ->
            Result
    end.

% --------------------------------------------------------------------
% Communication with the underlying module/socket.
% --------------------------------------------------------------------

%% @hidden

underlying_recv({Mod, Socket}, Timeout) ->
    Mod:recv(Socket, 0, Timeout).

%% @hidden

underlying_send({Mod, Socket}, Packet) ->
    Mod:send(Socket, Packet).

% --------------------------------------------------------------------
% gen_server(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden

init([]) ->
    Engines = dict:new(),
    By_AM = dict:new(),
    {ok, #state{engines = Engines, by_auth_method = By_AM}}.

%% @hidden

handle_call(get_auth_methods, _From,
  #state{by_auth_method = By_AM} = State) ->
    {reply, dict:fetch_keys(By_AM), State};

handle_call(get_engine_names, _From,
  #state{engines = Engines} = State) ->
    {reply, dict:fetch_keys(Engines), State};

handle_call({get_engines, AM}, _From,
  #state{by_auth_method = By_AM} = State) ->
    case dict:is_key(AM, By_AM) of
        true  -> {reply, [E || {E, _P} <- dict:fetch(AM, By_AM)], State};
        false -> {reply, [], State}
    end;

handle_call({get_engine, Engine_Name}, _From,
  #state{engines = Engines} = State) ->
    case dict:is_key(Engine_Name, Engines) of
        true  -> {reply, dict:fetch(Engine_Name, Engines), State};
        false -> {reply, undefined, State}
    end;

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
      "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

%% @hidden

handle_cast({register_engine,
  #tls_engine{name = Name, auth_methods = Auth_Methods} = Engine},
  #state{engines = Engines, by_auth_method = By_AM} = State) ->
    % Add engine to the global list.
    New_Engines = dict:store(Name, Engine, Engines),
    % Index engine by its auth methods.
    Fun = fun({AM, Prio}, {E, AM_Dict}) ->
        New_AM_Dict = case dict:is_key(AM, AM_Dict) of
            true ->
                L = [{E, Prio} | dict:fetch(AM, AM_Dict)],
                New_L = lists:keysort(2, L),
                dict:store(AM, New_L, AM_Dict);
            false ->
                dict:store(AM, [{E, Prio}], AM_Dict)
        end,
        {E, New_AM_Dict}
    end,
    {_, New_By_AM} = lists:foldl(Fun, {Engine, By_AM}, Auth_Methods),
    {noreply, State#state{
      engines = New_Engines,
      by_auth_method = New_By_AM
    }};

handle_cast(Request, State) ->
    error_logger:info_msg("~p:handle_cast/2:~n- Request: ~p~n"
      "- State: ~p~n", [?MODULE, Request, State]),
    {noreply, State}.

%% @hidden

handle_info(Info, State) ->
    error_logger:info_msg("~p:handle_info/2:~n- Info: ~p~n"
      "- State: ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

%% @hidden

code_change(Old_Vsn, State, Extra) ->
    error_logger:info_msg("~p:code_change/3:~n- Old_Vsn: ~p~n- Extra: ~p~n"
      "- State: ~p~n", [?MODULE, Old_Vsn, Extra, State]),
    {ok, State}.

%% @hidden

terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type tls_socket().
%% TLS socket obtained with {@link connect/4} or {@link accept/4}.
