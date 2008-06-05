% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle
%% stream compression.

-module(exmpp_compress).
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
  get_compress_methods/0,
  get_engine_names/0,
  get_engine_names/1,
  get_prefered_engine_name/1,
  get_engine_driver/1
]).

% Compression activation.
-export([
  enable_compression/2,
  disable_compression/1
]).

% Common socket API.
-export([
  send/2,
  recv/1,
  recv/2,
  recv_data/2,
  getopts/2,
  setopts/2,
  peername/1,
  sockname/1,
  controlling_process/2,
  close/1,
  port_revision/1
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
  by_compress_method
}).

-record(compress_engine, {
  name,
  driver_path,
  driver,
  compress_methods = []
}).

-record(compress_socket, {
  socket,
  packet_mode = binary,
  port
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_ENGINE, zlib).

-define(COMMAND_SET_COMPRESS_METHOD, 1).
-define(COMMAND_SET_COMPRESS_LEVEL,  2).
-define(COMMAND_PREPARE_COMPRESS,    3).
-define(COMMAND_PREPARE_UNCOMPRESS,  4).
-define(COMMAND_COMPRESS,            5).
-define(COMMAND_UNCOMPRESS,          6).
-define(COMMAND_SVN_REVISION,        7).

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

-ifdef(HAVE_ZLIB).
-define(REGISTER_ZLIB,
  register_engine(zlib, exmpp_compress_zlib, [{zlib, 10}, {gzip, 10}])).
-else.
-define(REGISTER_ZLIB, ok).
-endif.

register_builtin_engines() ->
    ?REGISTER_ZLIB,
    ok.

% --------------------------------------------------------------------
% Registry handling.
% --------------------------------------------------------------------

%% @spec (Name, Driver, Compress_Methods) -> ok
%%     Name = atom()
%%     Driver = atom()
%%     Compress_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new compression engine.

register_engine(Name, Driver, Compress_Methods) ->
    register_engine(Name, undefined, Driver, Compress_Methods).

%% @spec (Name, Driver_Path, Driver, Compress_Methods) -> ok
%%     Name = atom()
%%     Driver_Path = string()
%%     Driver = atom()
%%     Compress_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new compression engine.

register_engine(Name, Driver_Path, Driver, Compress_Methods)
  when is_atom(Name), is_list(Compress_Methods), length(Compress_Methods) > 0 ->
    Engine = #compress_engine{
      name = Name,
      driver_path = Driver_Path,
      driver = Driver,
      compress_methods = Compress_Methods
    },
    gen_server:cast(?SERVER, {register_engine, Engine}).

%% @spec () -> [Compress_Method]
%%     Compress_Method = atom()
%% @doc Return the list of supported compress methods.

get_compress_methods() ->
    gen_server:call(?SERVER, get_compress_methods).

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of compression engines.

get_engine_names() ->
    gen_server:call(?SERVER, get_engine_names).

%% @spec (Compress_Method) -> [Engine_Name]
%%     Compress_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the list of compression engines which support the given compress method.
%%
%% The list is sorted from the most to the least prefered engine.

get_engine_names(Compress_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Compress_Method}),
    [E#compress_engine.name || E <- Engines].

%% @spec (Compress_Method) -> [Engine_Name]
%%     Compress_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the name of the prefered compression engines which support the
%% given compress method.

get_prefered_engine_name(Compress_Method) ->
    case get_prefered_engine(Compress_Method) of
        undefined -> undefined;
        Engine    -> Engine#compress_engine.name
    end.

get_prefered_engine(Compress_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Compress_Method}),
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
        #compress_engine{driver_path = undefined, driver = Driver_Name} ->
            Driver_Name;
        #compress_engine{driver_path = Driver_Path, driver = Driver_Name} ->
            {Driver_Path, Driver_Name}
    end.

% --------------------------------------------------------------------
% Compression activation.
% --------------------------------------------------------------------

%% @spec (Socket_Desc, Options) -> Compress_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Options = [Option]
%%     Option = {compress_method, Method} | {engine, Engine} | {mode, Mode} | {compress_level, Level}
%%     Method = atom()
%%     Engine = atom()
%%     Mode = binary | list
%%     Level = integer()
%%     Compress_Socket = compress_socket()
%% @doc Enable compression over the given socket.

enable_compression(Socket_Desc, Options) ->
    % Start a port driver instance.
    Driver_Name = get_engine_from_options(Options),
    Port = try
        exmpp_internals:open_port(Driver_Name)
    catch
        Exception1 ->
            exmpp_internals:unload_driver(Driver_Name),
            throw(Exception1)
    end,

    % Initialize the port and handshake.
    try
        % Set compression method.
        case get_compress_method_from_options(Options) of
            undefined -> ok;
            CM        -> engine_set_compress_method(Port, CM)
        end,

        % Set compression level.
        case get_compress_level_from_options(Options) of
            undefined -> ok;
            Level     -> engine_set_compress_level(Port, Level)
        end,

        % Packet mode.
        Packet_Mode = get_packet_mode_from_options(Options),

        % Enable compression.
        engine_prepare_compress(Port),
        engine_prepare_uncompress(Port),
        #compress_socket{socket = Socket_Desc, packet_mode = Packet_Mode,
          port = Port}
    catch
        Exception2 ->
            exmpp_internals:close_port(Port),
            exmpp_internals:unload_driver(Driver_Name),
            throw(Exception2)
    end.

%% @spec (Compress_Socket) -> Socket_Desc
%%     Compress_Socket = compress_socket()
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%% @doc Disable compression and return the underlying socket.

disable_compression(#compress_socket{socket = Socket_Desc, port = Port}) ->
    exmpp_internals:close_port(Port),
    Socket_Desc.

% --------------------------------------------------------------------
% Activation helpers.
% --------------------------------------------------------------------

get_engine_from_options(Options) ->
    Engine_Name = case lists:keysearch(engine, 1, Options) of
        {value, {_, Engine}} ->
            Engine;
        _ ->
            case lists:keysearch(compress_method, 1, Options) of
                {value, {_, CM}} ->
                    get_prefered_engine_name(CM);
                _ ->
                    ?DEFAULT_ENGINE
            end
    end,
    case get_engine_driver(Engine_Name) of
        {Driver_Path, Driver_Name} ->
            exmpp_internals:load_driver(Driver_Name, [Driver_Path]),
            Driver_Name;
        Driver_Name ->
            exmpp_internals:load_driver(Driver_Name),
            Driver_Name
    end.

get_compress_method_from_options(Options) ->
    case lists:keysearch(compress_method, 1, Options) of
        {value, {_, CM}} -> CM;
        _                -> undefined
    end.

get_compress_level_from_options(Options) ->
    case lists:keysearch(compress_level, 1, Options) of
        {value, {_, Level}} when is_integer(Level) -> Level;
        _                                          -> default
    end.

get_packet_mode_from_options(Options) ->
    case lists:keysearch(mode, 1, Options) of
        {value, {_, binary}} -> binary;
        {value, {_, list}}   -> list;
        _                    -> binary
    end.

% --------------------------------------------------------------------
% Common socket API.
% --------------------------------------------------------------------

%% @spec (Compress_Socket, Orig_Packet) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Send `Orig_Packet' over a compressed connection.

send(#compress_socket{socket = Socket_Desc, port = Port}, Packet) ->
    try
        Compressed = engine_compress(Port, Packet),
        exmpp_internals:gen_send(Socket_Desc, Compressed)
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(Compress_Socket) ->
    recv(Compress_Socket, infinity).

%% @spec (Compress_Socket, Timeout) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Timeout = integer()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(#compress_socket{socket = Socket_Desc} = Compress_Socket, Timeout) ->
    try
        case exmpp_internals:gen_recv(Socket_Desc, Timeout) of
            {ok, Packet} ->
                recv_data(Compress_Socket, Packet);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Packet) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Packet = binary() | list()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Uncompressed already received data.

recv_data(#compress_socket{port = Port, packet_mode = Packet_Mode}, Packet) ->
    try
        Uncompressed = engine_uncompress(Port, Packet),
        case Packet_Mode of
            binary -> {ok, Uncompressed};
            list   -> {ok, binary_to_list(Uncompressed)}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Options) -> {ok, Option_Values} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%%     Option_Values = list()
%% @doc Sets one or more options for a socket.

getopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_getopts(Socket_Desc, Options).

%% @spec (Compress_Socket, Options) -> ok | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%% @doc Sets one or more options for a socket.

setopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_setopts(Socket_Desc, Options).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the address and port for the other end of a connection.

peername(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_peername(Socket_Desc).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the local address and port number for a socket.

sockname(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_sockname(Socket_Desc).

%% @spec (Compress_Socket, Pid) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Pid = pid()
%%     Reason = term()
%% @doc Change the controlling socket of the underlying socket.

controlling_process(#compress_socket{socket = Socket_Desc}, Pid) ->
    exmpp_internals:gen_controlling_process(Socket_Desc, Pid).

%% @spec (Compress_Socket) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Reason = term()
%% @doc Close the underlying socket.

close(#compress_socket{socket = Socket_Desc}) ->
    % Close the underlying socket.
    exmpp_internals:gen_close(Socket_Desc).

%% @hidden

port_revision(#compress_socket{port = Port}) ->
    engine_svn_revision(Port).

% --------------------------------------------------------------------
% Engine function wrappers.
% --------------------------------------------------------------------

control(Port, Command, Data) ->
    case port_control(Port, Command, Data) of
        <<0, Result/binary>> -> Result;
        <<1, Error/binary>>  -> {error, binary_to_term(Error)}
    end.

engine_set_compress_method(Port, Method) ->
    case control(Port, ?COMMAND_SET_COMPRESS_METHOD,
      term_to_binary(Method)) of
        {error, Reason} ->
            throw({compress, compress, set_compress_method, Reason});
        _ ->
            ok
    end.

engine_set_compress_level(Port, Level) ->
    case control(Port, ?COMMAND_SET_COMPRESS_LEVEL,
      term_to_binary(Level)) of
        {error, Reason} ->
            throw({compress, compress, set_compress_level, Reason});
        _ ->
            ok
    end.

engine_prepare_compress(Port) ->
    case control(Port, ?COMMAND_PREPARE_COMPRESS, <<>>) of
        {error, Reason} ->
            throw({compress, compress, prepare_compress, Reason});
        _ ->
            ok
    end.

engine_prepare_uncompress(Port) ->
    case control(Port, ?COMMAND_PREPARE_UNCOMPRESS, <<>>) of
        {error, Reason} ->
            throw({compress, compress, prepare_uncompress, Reason});
        _ ->
            ok
    end.

engine_compress(Port, Data) when is_list(Data) ->
    engine_compress(Port, list_to_binary(Data));
engine_compress(_Port, <<>>) ->
    <<>>;
engine_compress(Port, Data) ->
    case control(Port, ?COMMAND_COMPRESS, Data) of
        {error, Reason} ->
            throw({compress, compress, do_compress, Reason});
        Result ->
            Result
    end.

engine_uncompress(Port, Data) when is_list(Data) ->
    engine_uncompress(Port, list_to_binary(Data));
engine_uncompress(_Port, <<>>) ->
    <<>>;
engine_uncompress(Port, Data) ->
    case control(Port, ?COMMAND_UNCOMPRESS, Data) of
        {error, Reason} ->
            throw({compress, uncompress, do_uncompress, Reason});
        Result ->
            Result
    end.

engine_svn_revision(Port) ->
    case control(Port, ?COMMAND_SVN_REVISION, <<>>) of
        {error, Reason} ->
            throw({compress, handshake, svn_revision, Reason});
        Revision ->
            binary_to_term(Revision)
    end.

% --------------------------------------------------------------------
% gen_server(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden

init([]) ->
    Engines = dict:new(),
    By_CM = dict:new(),
    {ok, #state{engines = Engines, by_compress_method = By_CM}}.

%% @hidden

handle_call(get_compress_methods, _From,
  #state{by_compress_method = By_CM} = State) ->
    {reply, dict:fetch_keys(By_CM), State};

handle_call(get_engine_names, _From,
  #state{engines = Engines} = State) ->
    {reply, dict:fetch_keys(Engines), State};

handle_call({get_engines, CM}, _From,
  #state{by_compress_method = By_CM} = State) ->
    case dict:is_key(CM, By_CM) of
        true  -> {reply, [E || {E, _P} <- dict:fetch(CM, By_CM)], State};
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
  #compress_engine{name = Name, compress_methods = Compress_Methods} = Engine},
  #state{engines = Engines, by_compress_method = By_CM} = State) ->
    % Add engine to the global list.
    New_Engines = dict:store(Name, Engine, Engines),
    % Index engine by its compress methods.
    Fun = fun({CM, Prio}, {E, CM_Dict}) ->
        New_CM_Dict = case dict:is_key(CM, CM_Dict) of
            true ->
                L = [{E, Prio} | dict:fetch(CM, CM_Dict)],
                New_L = lists:keysort(2, L),
                dict:store(CM, New_L, CM_Dict);
            false ->
                dict:store(CM, [{E, Prio}], CM_Dict)
        end,
        {E, New_CM_Dict}
    end,
    {_, New_By_CM} = lists:foldl(Fun, {Engine, By_CM}, Compress_Methods),
    {noreply, State#state{
      engines = New_Engines,
      by_compress_method = New_By_CM
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

%% @type compress_socket().
%% Compression socket obtained with {@link compress/2}.
