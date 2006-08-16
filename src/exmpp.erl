% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> currently provide some generic
%% utilities for other modules of this application.
%%
%% <p>
%% It's not intended to be used directly.
%% </p>

-module(exmpp).
-vsn('$Revision$').

-export([version/0]).

% --------------------------------------------------------------------
% Generic utilities.
% --------------------------------------------------------------------

%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
	{ok, Version} = application:get_key(exmpp, vsn),
	Version.
