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
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of the TLS feature.
%%
%% <p>
%% Note that it doesn't implement encryption, only feature negotiation
%% at the XMPP level.
%% </p>

-module(exmpp_server_tls).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    feature/0,
    feature/1
]).

%% TLS negotiation.
-export([
    proceed/0,
    failure/0
]).

%%
-export_type([
    xmlel_starttls/0,
    xmlel_required/0,
    xmlel_proceed/0,
    xmlel_failure/0
]).

-type(xmlel_starttls()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_TLS :: <<_:248>>},...],
         children :: []
                   | [Xmlel_Required::exmpp_server_tls:xmlel_required(),...]
     }
).

-type(xmlel_required()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [],
         children :: []
     }
).

-type(xmlel_proceed()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_TLS :: <<_:248>>},...],
         children :: []
     }
).

-type(xmlel_failure()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_TLS :: <<_:248>>},...],
         children :: []
     }
).


%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).


-define(Xmlel@TLS(Name, Attrs, Children),
(
    exxml:element(?NS_TLS, Name, Attrs, Children)
)).


%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @doc Make a feature announcement child.
%%
%% TLS is announced as not required.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.
%%
%% @see feature/1.
-spec(feature/0 :: () -> Xmlel_Starttls::exmpp_server_tls:xmlel_starttls()).

feature() ->
    feature(false).

%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.
-spec(feature/1 ::
(
  Is_Required::boolean())
    -> Xmlel_Starttls::exmpp_server_tls:xmlel_starttls()
).

feature(Is_Required) ->
    ?Xmlel@TLS(<<"starttls">>, [],
        [?Xmlel(<<"required">>, [], []) || Is_Required]).

%% --------------------------------------------------------------------
%% TLS negotiation.
%% --------------------------------------------------------------------

%% @doc Make an XML element to tell the initiating entity it can proceed
%% with the TLS handshake.
-spec(proceed/0 :: () -> Xmlel_Proceed::exmpp_server_tls:xmlel_proceed()).

proceed() ->
    ?Xmlel@TLS(<<"proceed">>, [], []).

%% @doc Make an XML element to tell the initiating entity that the TLS
%% handshake failed.
-spec(failure/0 :: () -> Xmlel_Proceed::exmpp_server_tls:xmlel_failure()).

failure() ->
    ?Xmlel@TLS(<<"failure">>, [], []).
