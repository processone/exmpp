% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> is the master supervisor.
%%
%% <p>
%% It will start the following services:
%% </p>
%% <ol>
%%   <li>{@link exmpp_stringprep}</li>
%% </ol>
%%
%% <p>
%% It's not intended to be used directly.
%% </p>

-module(exmpp_sup).
-vsn('$Revision$').

-behaviour(supervisor).

% Initialization.
-export([
  start/0,
  start_link/0
]).

% supervisor(3erl) callbacks.
-export([
  init/1
]).

-define(SUPERVISOR, ?MODULE).

% --------------------------------------------------------------------
% Public API.
% --------------------------------------------------------------------

%% @spec () -> Result
%%     Result = term()
%% @doc Start the supervisor.
%%
%% @see supervisor:start/3.

start() ->
    supervisor:start({local, ?SUPERVISOR}, ?MODULE, []).

%% @spec () -> Result
%%     Result = term()
%% @doc Start the supervisor and link to it.
%%
%% @see supervisor:start_link/3.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

% --------------------------------------------------------------------
% supervisor(3erl) callbacks.
% --------------------------------------------------------------------

%% @hidden

init(_Args) ->
    % Stringprep.
    Stringprep = {stringprep,
      {exmpp_stringprep, start_link, []},
      transient,
      2000,
      worker,
      [exmpp_stringprep]
    },
    {ok, {
      {one_for_one, 10, 1}, [
        Stringprep
      ]
    }}.
