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
%% entity side of the TLS feature.
%%
%% <p>
%% Note that it doesn't implement encryption, only feature negotiation
%% at the XMPP level.
%% </p>

-module(exmpp_client_tls).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    announced_support/1
]).

%% TLS negotiation.
-export([
    starttls/0
]).

-export_type([
  xmlel_starttls/0
]).

-type(xmlel_starttls()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_TLS :: <<_:248>>},...],
         children :: []
     }
).

%%
-define(Xmlel@TLS(Name, Attrs, Children),
(
    exxml:element(?NS_TLS, Name, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @throws {tls, announced_support, invalid_announcement, El}
%% @doc Return the kind of TLS negotiation the receiving entity asks for.
-spec(announced_support/1 ::
(
  Xmlel_Features :: exmpp_stream:xmlel_features())
    -> Support :: none | 'optional' | 'required'
).

announced_support(Xmlel_Features)
  when   Xmlel_Features#xmlel.name == <<"features">>
  orelse Xmlel_Features#xmlel.name == <<"stream:features">> ->
    case exxml:get_el(Xmlel_Features, <<"starttls">>) of
        undefined      -> none;
        Xmlel_Starttls -> announced_support2(Xmlel_Starttls)
    end.

%%
-spec(announced_support2/1 ::
(
  Xmlel_Starttls :: exmpp_server_tls:xmlel_starttls())
    -> Support :: 'optional' | 'required'
).

announced_support2(Xmlel_Starttls) ->
    case exxml:get_els(Xmlel_Starttls) of
        [] ->
            'optional';
        [#xmlel{name = <<"required">>, children = []}] ->
            'required';
        _ ->
            throw({tls, announced_support, 'invalid_announcement', Xmlel_Starttls})
    end.

%% --------------------------------------------------------------------
%% TLS negotiation.
%% --------------------------------------------------------------------

%% @doc Make an XML element to tell the receiving entity that we want to
%% use TLS.
-spec(starttls/0 :: () -> Xmlel_Starttls::exmpp_client_tls:xmlel_starttls()).

starttls() ->
    ?Xmlel@TLS(<<"starttls">>, [], []).
