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
%% side of Resource Binding.

-module(exmpp_server_binding).

-include("exmpp.hrl").

%% Feature annoucement.
-export([
    feature/0
]).

%% Resource binding.
-export([
    wished_resource/1,
    bind/2,
    error/2
]).


%%
-export_type([
    xmlel_bind/0
]).

-type(xmlel_bind()
  :: #xmlel{
         name     :: <<_:32>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_BIND :: <<_:256>>}],
         children :: []
     }
).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@Bind(Name, Attrs, Children),
(
    exxml:element(?NS_BIND, Name, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

-spec(feature/0 :: () -> Xmlel_Bind::exmpp_server_binding:xmlel_bind()).

feature() ->
    ?Xmlel@Bind(<<"bind">>, [], []).

%% --------------------------------------------------------------------
%% Resource binding.
%% --------------------------------------------------------------------

%% @throws {resource_binding, wished_resource, invalid_bind, IQ}
%% @doc Return the resource the client wants or `undefined' if he
%% doesn't ask for any.
-spec(wished_resource/1 ::
(
  Stanza_IQ :: exmpp_stanza:iq_set() | exmpp_stanza:iq())
    -> Resource :: binary() | undefined
).

wished_resource(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case exmpp_iq:get_type(Stanza_IQ) of
        <<"set">> ->
            case exmpp_iq:get_request(Stanza_IQ) of
                Xmlel_Bind when Xmlel_Bind#xmlel.name == <<"bind">> ->
                    case exxml:get_el(Xmlel_Bind, <<"resource">>) of
                        undefined -> undefined;
                        Resource  -> exxml:get_cdata(Resource)
                    end;
                _ ->
                    throw({resource_binding, wished_resource, invalid_bind, Stanza_IQ})
            end;
        _ ->
            throw({resource_binding, wished_resource, invalid_bind, Stanza_IQ})
    end;
wished_resource(Stanza) ->
    throw({resource_binding, wished_resource, invalid_bind, Stanza}).

%% @doc Prepare a reply to `IQ' to inform the client of its final JID.
-spec(bind/2 ::
(
  Stanza_IQ :: exmpp_stanza:iq_set(),
  Jid       :: exmpp_jid:jid())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result()
).

bind(Stanza_IQ, Jid) when ?IS_IQ(Stanza_IQ) ->
    exmpp_iq:result(Stanza_IQ,
        ?Xmlel@Bind(<<"bind">>, [], [
            ?Xmlel(<<"jid">>, [], [exxml:cdata(exmpp_jid:to_binary(Jid))])
        ])
    ).

%% @doc Prepare an error reply to `IQ'.
-spec(error/2 ::
(
  Stanza_IQ       :: exmpp_stanza:iq(),
  Error_Condition :: exmpp_stanza:error_condition())
    -> Stanza_IQ_Error::exmpp_stanza:iq_error()
).

error(Stanza_IQ, Error_Condition) when ?IS_IQ(Stanza_IQ) ->
    exmpp_iq:error(Stanza_IQ, exmpp_stanza:error(Error_Condition)).
