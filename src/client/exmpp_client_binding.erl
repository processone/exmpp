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
%% entity side of Resource Binding.

-module(exmpp_client_binding).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    announced_support/1
]).

%% Resource binding.
-export([
    bind/0,
    bind/1,
    bounded_jid/1
]).

-export_type([
    xmlel_bind/0
]).

-type(xmlel_bind()
  :: #xmlel{
         name     :: <<_:40>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_BIND :: <<_:256>>}],
         children :: []
     }
).


-define(Xmlel@Bind(Name, Attrs, Children),
(
    exxml:element(?NS_BIND, Name, Attrs, Children)
%    #xmlel{
%        name     = Name,
%        attrs    = [{<<"xmlns">>, ?NS_BIND} | Attrs],
%        children = Children
%    }
)).


%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> bool()
%%     Features_Announcement = exxml:xmlel()()
%% @throws {resource_binding, announced_support, invalid_feature, Feature}
%% @doc Tell if the Resource Binding feature is supported.

-spec(announced_support/1 ::
(
  Xmlel_Features :: exmpp_stream:xmlel_features())
    -> boolean()
).

announced_support(Xmlel_Features)
  when Xmlel_Features#xmlel.name == <<"features">> ->
    case exxml:get_element(Xmlel_Features, <<"bind">>) of
        undefined  -> false;
        Xmlel_Bind -> announced_support2(Xmlel_Bind)
    end.

-spec(announced_support2/1 ::
(
  Xmlel_Bind :: exmpp_client_binding:xmlel_bind())
    -> true
).

announced_support2(#xmlel{name = <<"bind">>, children = []}) ->
    true;
announced_support2(Xmlel_Bind) ->
    throw({resource_binding, announced_support, invalid_feature, Xmlel_Bind}).

%% --------------------------------------------------------------------
%% Resource binding.
%% --------------------------------------------------------------------

%% @spec () -> Bind
%%     Bind = exxml:xmlel()
%% @doc Prepare a Resource Binding request.

-spec(bind/0 :: () -> Stanza_IQ_Set::exmpp_stanza:stanza_iq_set()).

bind() ->
    bind(undefined).

%% @spec (Resource) -> Bind
%%     Bind = exxml:xmlel()
%% @doc Prepare a Resource Binding request for the given `Resource'.

-spec(bind/1 ::
(
  Resource :: binary() | undefined)
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

bind(Resource) ->
    exmpp_iq:set(
        ?Xmlel@Bind(<<"bind">>, [],
            case Resource of
                undefined ->
                    [];
                _ ->
                    [?Xmlel@Bind(<<"resource">>, [], [exxml:cdata(Resource)])]
            end),
        exmpp_utils:random_id(<<"bind">>)).

%% @spec (Bind) -> Jid
%%     Bind = exxml:xmlel()
%%     Jid = exmpp_jid:jid()
%% @throws {resource_binding, bounded_jid, invalid_bind, Stanza} |
%%         {resource_binding, bounded_jid, no_jid, IQ} |
%%         {resource_binding, bounded_jid, bind_error, Condition}
%% @doc Extract the JID given by the server.

-spec(bounded_jid/1 ::
(
  Stanza_IQ :: exmpp_stanza:iq_result() | exmpp_stanza:stanza())
    -> Jid :: exmpp_jid:jid()
).

bounded_jid(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case exmpp_iq:get_type(Stanza_IQ) of
        <<"result">> ->
            case exmpp_iq:get_result(Stanza_IQ) of
                Xmlel_Bind when Xmlel_Bind#xmlel.name == <<"bind">> ->
                    case exxml:get_element(Xmlel_Bind, <<"jid">>) of
                        undefined ->
                            throw({resource_binding, bounded_jid, no_jid, Stanza_IQ});
                        Xmlel_Jid ->
                            exmpp_jid:parse(exxml:get_cdata(Xmlel_Jid))
                    end;
                _ ->
                    throw({resource_binding, bounded_jid, no_jid, Stanza_IQ})
            end;
        <<"error">> ->
            Error_Condition = exmpp_stanza:get_condition(Stanza_IQ),
            throw({resource_binding, bounded_jid, bind_error, Error_Condition})
    end;
bounded_jid(Stanza) ->
    throw({resource_binding, bounded_jid, invalid_bind, Stanza}).
