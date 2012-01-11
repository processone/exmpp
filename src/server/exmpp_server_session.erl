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
%% side of the Session Establishment.

-module(exmpp_server_session).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    feature/0
]).

%% Session establishment.
-export([
    want_establishment/1,
    establish/1,
    error/2
]).

%%
-export_type([
    xmlel_session/0
]).

-type(xmlel_session()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_SESSION :: <<_:280>>},...],
         children :: []
     }
).

%%
-define(Xmlel@Session(Name, Attrs, Children),
(
    exxml:element(?NS_SESSION, Name, Attrs, Children)
)).


%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @doc Make a feature annoucement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.
-spec(feature/0 :: () -> Xmlel_Session::exmpp_server_session:xmlel_session()).

feature() ->
    ?Xmlel@Session(<<"session">>, [], []).

%% --------------------------------------------------------------------
%% Session establishment.
%% --------------------------------------------------------------------

%% @doc Tell if the initiating entity wants to establish a session.
-spec(want_establishment/1 ::
(
  Stanza_IQ_Set::exmpp_stanza:iq_set())
    -> Want_Establishment::boolean()
).

want_establishment(Stanza_IQ_Set) when ?IS_IQ(Stanza_IQ_Set) ->
    case exmpp_iq:get_type(Stanza_IQ_Set) of
        <<"set">> ->
            case exmpp_iq:get_payload_ns(Stanza_IQ_Set) of
                ?NS_SESSION -> true;
                _           -> false
            end;
        _ ->
            false
    end;
want_establishment(_Stanza) ->
    false.

%% @doc Prepare a result IQ to inform the initiating entity that the
%% session is created.
-spec(establish/1 ::
(
  Stanza_IQ_Set::exmpp_stanza:iq_set())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result()
).

establish(Stanza_IQ_Set) when ?IS_IQ(Stanza_IQ_Set) ->
    exmpp_iq:result(Stanza_IQ_Set).

%% @doc Prepare an error reply to `IQ'.
-spec(error/2 ::
(
  Stanza_IQ_Set   :: exmpp_stanza:iq_set(),
  Error_Condition :: exmpp_stream:error_condition())
    -> Stanza_IQ_Error::exmpp_stanza:iq_error()
).

error(Stanza_IQ, Error_Condition) when ?IS_IQ(Stanza_IQ) ->
    exmpp_iq:error(Stanza_IQ, exmpp_stanza:error(Error_Condition)).
