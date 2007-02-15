% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides callbacks for
%% application(3) as well as some generic utilities for other modules of
%% this application.
%%
%% <p>
%% It's not intended to be used directly.
%% </p>

-module(exmpp).
-vsn('$Revision$').

-behaviour(application).

-export([
  start/0,
  version/0
]).

% application(3erl) callbacks.
-export([
  start/2,
  stop/1,
  config_change/3
]).

% --------------------------------------------------------------------
% Generic utilities.
% --------------------------------------------------------------------

%% @spec () -> ok
%% @doc Start applications which exmpp depends on then start exmpp.
start() ->
    application:start(exmpp).

%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
	{ok, Version} = application:get_key(exmpp, vsn),
	Version.

% --------------------------------------------------------------------
% application(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden
start(_Start_Type, _Start_Args) ->
    exmpp_sup:start_link().

%% @hidden
stop(_State) ->
    ok.

%% @hidden
config_change(Changed, New, Removed) ->
    error_logger:info_msg("Config change:~n"
      "Changed: ~p~n"
      "New: ~p~n"
      "Removed: ~p~n",
      [Changed, New, Removed]),
    ok.
