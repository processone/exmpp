% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides internal functions for
%% other modules.

-module(exmpp_internals).
-vsn('$Revision$').

-export([
  load_driver/1,
  unload_driver/1,
  open_port/1,
  close_port/1
]).

% --------------------------------------------------------------------
% Port driver loading/unloading.
% --------------------------------------------------------------------

driver_dirs() ->
    Dirs = ["priv/lib", "../priv/lib"],
    case code:priv_dir(exmpp) of
        {error, _Reason} -> Dirs;
        Priv_Dir         -> Dirs ++ [Priv_Dir ++ "/lib"]
    end.

load_driver(Driver_Name) ->
    Dirs = driver_dirs(),
    load_driver1(Driver_Name, Dirs, undefined).

load_driver1(Driver_Name, [Dir | Rest], _Reason) ->
    case erl_ddll:load_driver(Dir, Driver_Name) of
        ok ->
            ok;
        {error, Reason} ->
            load_driver1(Driver_Name, Rest, Reason)
    end;

load_driver1(_DriverName, [], Reason) ->
    error_logger:info_msg([{error, Reason}]),
    {error, Reason}.

-ifdef(WITH_BROKEN_ERL_DDLL).
unload_driver(_Driver_Name) ->
    % At least until R11B-1, erl_ddll messes up its libraries reference
    % count, so we can't unload the driver.
    ok.
-else.
unload_driver(Driver_Name) ->
    erl_ddll:unload_driver(Driver_Name).
-endif.

open_port(Driver_Name) ->
    case catch erlang:open_port({spawn, atom_to_list(Driver_Name)}, []) of
        {'EXIT', _Port, Posix_Code} ->
            {error, Posix_Code};
        Port ->
            Port
    end.

close_port(Port) ->
    erlang:port_close(Port).
