% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides internal functions for
%% other modules.

-module(exmpp_internals).
-vsn('$Revision$').

-export([
  driver_dirs/0,
  load_driver/1,
  unload_driver/1,
  open_port/1,
  close_port/1
]).
-export([
  encode_base64/1,
  decode_base64/1
]).

% --------------------------------------------------------------------
% Port driver loading/unloading.
% --------------------------------------------------------------------

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

-ifdef(ENABLE_ERL_DDLL_WORKAROUND).
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
        {'EXIT', {PosixCode, _Stack}} ->
	    ErrorType = port_error,
	    exmpp_error:throw(ErrorType,
			      PosixCode,
			      "Cannot open driver '~p': ~s",
			      [Driver_Name, 
			       exmpp_error:posix_message(ErrorType,
							 PosixCode)]);
        Port ->
            Port
    end.

close_port(Port) ->
    erlang:port_close(Port).

% --------------------------------------------------------------------
% Utils.
% --------------------------------------------------------------------

-ifdef(ENABLE_HTTP_BASE_64).
% Starting with inets 5.0, http_base_64 doesn't exist anymore.
encode_base64(Data) ->
    case catch http_base_64:encode(Data) of
        {'EXIT', _} -> base64:encode(Data);
        Base64      -> Base64
    end.

decode_base64(Data) ->
    case catch http_base_64:decode(Data) of
        {'EXIT', _} -> base64:decode(Data);
        Base64      -> Base64
    end.
-else.
encode_base64(Data) ->
    base64:encode(Data).

decode_base64(Data) ->
    base64:decode(Data).
-endif.
