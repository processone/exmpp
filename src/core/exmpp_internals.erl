% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides internal functions for
%% other modules.

-module(exmpp_internals).
-vsn('$Revision$').

% Port driver handling.
-export([
  driver_dirs/0,
  load_driver/1,
  load_driver/2,
  unload_driver/1,
  open_port/1,
  close_port/1
]).

% Generic socket handling.
-export([
  gen_recv/2,
  gen_send/2
]).

% Base64 helpers.
-export([
  encode_base64/1,
  decode_base64/1
]).

% Utils.
-export([
  random_id/0,
  random_id/1
]).

% --------------------------------------------------------------------
% Port driver loading/unloading.
% --------------------------------------------------------------------

%% @spec () -> Dirs | []
%%     Dirs = [string()]
%% @doc Return a list of directories to search port drivers in.

driver_dirs() ->
    Mod_Path = case code:is_loaded(?MODULE) of
        {file, preloaded} ->
            undefined;
        {file, cover_compiled} ->
            case code:is_loaded(check_coverity) of
                {file, preloaded}      -> undefined;
                {file, cover_compiled} -> undefined;
                {file, Path}           -> Path
            end;
        {file, Path} ->
            Path
    end,
    Dirs0 = case Mod_Path of
        undefined ->
            [];
        _ ->
            Base_Dir = filename:dirname(filename:dirname(Mod_Path)),
            [
              filename:join([Base_Dir, "priv", "lib"]),
              filename:join([Base_Dir, "c_src", ".libs"]),
              filename:join([Base_Dir, "c_src"])
            ]
    end,
    case code:priv_dir(exmpp) of
        {error, _Reason} -> Dirs0;
        Priv_Dir         -> Dirs0 ++ [filename:join(Priv_Dir, "lib")]
    end.

%% @spec (Driver_Name) -> ok
%%     Driver_Name = atom()
%% @throws {port_driver, load, Reason, Driver_Name}
%% @doc Load the port driver `Driver_Name'.

load_driver(Driver_Name) ->
    Dirs = driver_dirs(),
    load_driver(Driver_Name, Dirs).

%% @spec (Driver_Name, Dirs) -> ok
%%     Driver_Name = atom()
%%     Dirs = [string()]
%% @throws {port_driver, load, Reason, Driver_Name}
%% @doc Load the port driver `Driver_Name'.
%%
%% The driver is search in `Dirs'.

load_driver(Driver_Name, Dirs) ->
    load_driver1(Driver_Name, Dirs, undefined).

% This function will try to load `Driver_Name' from each `Dir' in the list.
load_driver1(Driver_Name, [Dir | Rest], _Reason) ->
    case erl_ddll:load_driver(Dir, Driver_Name) of
        ok ->
            ok;
        {error, Reason} ->
            % Next directory.
            load_driver1(Driver_Name, Rest, Reason)
    end;
load_driver1(Driver_Name, [], Reason) ->
    % We walk through each directories without being able to load the driver.
    throw({port_driver, load, Reason, Driver_Name}).

%% @spec (Driver_Name) -> ok
%%     Driver_Name = atom()
%% @doc Unload the port driver `Driver_Name'.

-ifdef(ENABLE_ERL_DDLL_WORKAROUND).
unload_driver(_Driver_Name) ->
    % At least until R11B-1, erl_ddll messes up its library references
    % count, so we can't unload the driver.
    ok.
-else.
unload_driver(Driver_Name) ->
    erl_ddll:unload_driver(Driver_Name),
    ok.
-endif.

%% @spec (Driver_Name) -> Port
%%     Driver_Name = atom()
%%     Port = port()
%% @throws {port_driver, open, {posix, Posix_Code}, Driver_Name}
%% @doc Spawn a new port driver instance.

open_port(Driver_Name) ->
    try
        erlang:open_port({spawn, atom_to_list(Driver_Name)}, [])
    catch
        exit:{Posix_Code, _Stack} ->
            throw({port_driver, open, {posix, Posix_Code}, Driver_Name})
    end.

%% @spec (Port) -> true
%%     Port = port()
%% @doc Close a previously spawned port.
%%
%% `Port' was obtained with {@link open_port/1}.

close_port(Port) ->
    erlang:port_close(Port).

% --------------------------------------------------------------------
% Generic socket handling.
% --------------------------------------------------------------------

%% @spec (Socket_Desc, Timeout) -> {ok, Packet} | {error, Reason}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Timeout = integer()
%%     Packet = [char()] | binary()
%%     Reason = closed | posix()
%% @doc Wrapper to abstract the recv function of multiple communication
%% modules.

gen_recv({gen_tcp, Socket}, Timeout) ->
    gen_tcp:recv(Socket, 0, Timeout);
gen_recv({Mod, Socket}, Timeout) ->
    Mod:recv(Socket, Timeout).

%% @spec (Socket_Desc, Packet) -> ok | {error, Reason}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Packet = [char()] | binary()
%%     Reason = closed | posix()
%% @doc Wrapper to abstract the send function of multiple communication
%% modules.

gen_send({Mod, Socket}, Packet) ->
    Mod:send(Socket, Packet).

% --------------------------------------------------------------------
% Base64 helpers.
% --------------------------------------------------------------------

%% @spec (Data) -> Base64
%%     Data = string()
%%     Base64 = string()
%% @doc Encode `Data' in Base64.

-ifdef(ENABLE_HTTP_BASE_64).
% Starting with inets 5.0, http_base_64 doesn't exist anymore.
encode_base64(Data) ->
    case catch http_base_64:encode(Data) of
        {'EXIT', _} -> base64:encode_to_string(Data);
        Base64      -> Base64
    end.
-else.
encode_base64(Data) ->
    base64:encode_to_string(Data).
-endif.

%% @spec (Base64) -> Data
%%     Base64 = string()
%%     Data = string()
%% @doc Decode `Base64' to get `Data' back.

-ifdef(ENABLE_HTTP_BASE_64).
% Starting with inets 5.0, http_base_64 doesn't exist anymore.
decode_base64(Base64) ->
    case catch http_base_64:decode(Base64) of
        {'EXIT', _} -> base64:decode_to_string(Base64);
        Data        -> Data
    end.
-else.
decode_base64(Base64) ->
    base64:decode_to_string(Base64).
-endif.

% --------------------------------------------------------------------
% Utils.
% --------------------------------------------------------------------

%% @spec () -> ID
%%     ID = string()
%% @doc Generate a random ID.
%%
%% Use the `exmpp' prefix.
%%
%% @see random_id/1.

random_id() ->
    random_id("exmpp").

%% @spec (Prefix) -> ID
%%     Prefix = string()
%%     ID = string()
%% @doc Generate a random stanza ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
%%
%% The ID is not guaranted to be unique.

random_id(Prefix) ->
    Prefix ++ "-" ++ integer_to_list(random:uniform(65536 * 65536)).
