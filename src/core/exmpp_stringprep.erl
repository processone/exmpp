% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to use
%% NODEPREP, NAMEPREP and RESOURCEPREP stringprep profiles.
%%
%% <p>
%% It's not intended to be used directly.
%% </p>

-module(exmpp_stringprep).
-vsn('$Revision$').

-behaviour(gen_server).

% Initialization.
-export([
  start/0,
  start_link/0
]).

% Stringprep profiles.
-export([
  nodeprep/1,
  nameprep/1,
  resourceprep/1
]).

% Tools.
-export([
  is_node/1,
  is_name/1,
  is_resource/1,
  to_lower/1,
  port_revision/0
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
  port
}).

-define(SERVER, ?MODULE).
-define(DRIVER_NAME, exmpp_stringprep).
-define(PORT_REGISTERED_NAME, exmpp_stringprep_port).

-define(COMMAND_LOWERCASE,    0).
-define(COMMAND_NAMEPREP,     1).
-define(COMMAND_NODEPREP,     2).
-define(COMMAND_RESOURCEPREP, 3).
-define(COMMAND_SVN_REVISION, 4).

% --------------------------------------------------------------------
% Initialization.
% --------------------------------------------------------------------

%% @hidden

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @hidden

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% --------------------------------------------------------------------
% Stringprep profiles.
% --------------------------------------------------------------------

%% @spec (String) -> Prepd_String
%%     String = string()
%%     Prepd_String = string()
%% @throws {stringprep, nodeprep, Reason, String}
%% @doc Apply the NODEPREP stringprep profile to `String'.

nodeprep(String) ->
    case control(?COMMAND_NODEPREP, String) of
        {error, Reason} ->
            throw({stringprep, nodeprep, Reason, String});
        Result ->
            Result
    end.

%% @spec (String) -> Prepd_String
%%     String = string()
%%     Prepd_String = string()
%% @throws {stringprep, nameprep, Reason, String}
%% @doc Apply the NAMEPREP stringprep profile to `String'.

nameprep(String) ->
    case control(?COMMAND_NAMEPREP, String) of
        {error, Reason} ->
            throw({stringprep, nameprep, Reason, String});
        Result ->
            Result
    end.

%% @spec (String) -> Prepd_String
%%     String = string()
%%     Prepd_String = string()
%% @throws {stringprep, resourceprep, Reason, String}
%% @doc Apply the RESOURCEPREP stringprep profile to `String'.

resourceprep(String) ->
    case control(?COMMAND_RESOURCEPREP, String) of
        {error, Reason} ->
            throw({stringprep, resourceprep, Reason, String});
        Result ->
            Result
    end.

% --------------------------------------------------------------------
% Tools.
% --------------------------------------------------------------------

%% @spec (String) -> bool()
%%     String = string()
%% @doc Tell if `String' conforms the NODEPREP stringprep profile.

is_node("") ->
    false;
is_node(String) ->
    try
        nodeprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> bool()
%%     String = string()
%% @doc Tell if `String' conforms the NAMEPREP stringprep profile.

is_name("") ->
    false;
is_name(String) ->
    try
        nameprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> bool()
%%     String = string()
%% @doc Tell if `String' conforms the RESOURCEPREP stringprep profile.

is_resource("") ->
    false;
is_resource(String) ->
    try
        resourceprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> bool()
%%     String = string()
%% @throws {stringprep, lowercase, Reason, String}
%% @doc Convert `String' to lowercase.

to_lower(String) ->
    case control(?COMMAND_LOWERCASE, String) of
        {error, Reason} ->
            throw({stringprep, lowercase, Reason, String});
        Result ->
            Result
    end.

%% @hidden

port_revision() ->
    control(?COMMAND_SVN_REVISION, "").

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

control(Command, String) ->
    case port_control(?PORT_REGISTERED_NAME, Command, String) of
        [0 | _]      -> {error, undefined};
        [1 | Result] -> Result
    end.

% --------------------------------------------------------------------
% gen_server(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden

init([]) ->
    try
        exmpp_internals:load_driver(?DRIVER_NAME),
        Port = exmpp_internals:open_port(?DRIVER_NAME),
        register(?PORT_REGISTERED_NAME, Port),
        State = #state{
          port = Port
        },
        {ok, State}
    catch
        throw:{port_driver, load, _, _} = Exception ->
            {stop, Exception};
        throw:{port_driver, open, _, _} = Exception ->
            exmpp_internals:unload_driver(?DRIVER_NAME),
            {stop, Exception}
    end.

%% @hidden

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
      "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

%% @hidden

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

terminate(_Reason, #state{port = Port} = _State) ->
    exmpp_internals:close_port(Port),
    exmpp_internals:unload_driver(?DRIVER_NAME),
    ok.
