% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to use
%% NODEPREP, NAMEPREP and RESOURCEPREP stringprep profiles.

-module(exmpp_stringprep).
-vsn('$Revision$').

-behaviour(gen_server).

-export([
	 start/0,
	 start_link/0,
	 nodeprep/1,
	 nameprep/1,
	 resourceprep/1,
	 is_node/1,
	 is_name/1,
	 is_resource/1,
	 to_lower/1
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
-define(DRIVER_NAME, exmpp_stringprep_drv).
-define(PORT_REGISTERED_NAME, exmpp_stringprep_port).

-define(COMMAND_LOWERCASE,    0).
-define(COMMAND_NAMEPREP,     1).
-define(COMMAND_NODEPREP,     2).
-define(COMMAND_RESOURCEPREP, 3).

% --------------------------------------------------------------------
% Initialization.
% --------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% --------------------------------------------------------------------
% Stringprep profiles.
% --------------------------------------------------------------------

nodeprep(String) ->
    control(?COMMAND_NODEPREP, String).

nameprep(String) ->
    control(?COMMAND_NAMEPREP, String).

resourceprep(String) ->
    control(?COMMAND_RESOURCEPREP, String).

% --------------------------------------------------------------------
% Tools.
% --------------------------------------------------------------------

is_node("") ->
    false;
is_node(String) ->
    nodeprep(String) /= error.

is_name("") ->
    false;
is_name(String) ->
    nameprep(String) /= error.

is_resource("") ->
    false;
is_resource(String) ->
    resourceprep(String) /= error.

to_lower(String) ->
    control(?COMMAND_LOWERCASE, String).

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

control(Command, String) ->
    case port_control(?PORT_REGISTERED_NAME, Command, String) of
        [0 | _]      -> error;
        [1 | Result] -> Result
    end.

% --------------------------------------------------------------------
% gen_server(3erl) callbacks.
% --------------------------------------------------------------------

init([]) ->
    case exmpp_internals:load_driver(?DRIVER_NAME) of
        {error, Reason} ->
            {stop, Reason};
        ok ->
            case exmpp_internals:open_port(?DRIVER_NAME) of
                {error, Reason} ->
                    exmpp_internals:unload_driver(?DRIVER_NAME),
                    {stop, Reason};
                Port ->
                    register(?PORT_REGISTERED_NAME, Port),
                    State = #state{
                      port = Port
                    },
                    {ok, State}
            end
    end.

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
      "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    error_logger:info_msg("~p:handle_cast/2:~n- Request: ~p~n"
      "- State: ~p~n", [?MODULE, Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("~p:handle_info/2:~n- Info: ~p~n"
      "- State: ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

code_change(Old_Vsn, State, Extra) ->
    error_logger:info_msg("~p:code_change/3:~n- Old_Vsn: ~p~n- Extra: ~p~n"
      "- State: ~p~n", [?MODULE, Old_Vsn, Extra, State]),
    {ok, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    exmpp_internals:close_port(Port),
    exmpp_internals:unload_driver(?DRIVER_NAME),
    ok.
