%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of the Session Establishment.

-module(exmpp_client_session).

-include("exmpp.hrl").


%% Session establishment.
-export([
    establish/0
]).

%% --------------------------------------------------------------------
%% Session establishment.
%% --------------------------------------------------------------------

%% @doc Make an IQ element with `<session/>' element to create a session.

-spec(establish/0 :: () -> Stanza_IQ_Set::exmpp_stanza:stanza_set()).

establish() ->
    ?IQ_SET(undefined, undefined, undefined,
        exxml:element(?NS_SESSION, <<"session">>, [], [])
    ).

