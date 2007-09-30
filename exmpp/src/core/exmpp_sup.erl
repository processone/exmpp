% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> is the master supervisor.

-module(exmpp_sup).
-vsn('$Revision$').

-behaviour(supervisor).

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

start() ->
    supervisor:start({local, ?SUPERVISOR}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

% --------------------------------------------------------------------
% supervisor(3erl) callbacks.
% --------------------------------------------------------------------

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
