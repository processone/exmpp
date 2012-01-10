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
%% side of Stream Compression (XEP-0138).
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0138.html">XEP-0138: Stream Compression</a>
%% @reference <a href="http://www.xmpp.org/extensions/xep-0229.html">XEP-0229: Stream Compression with LZW</a>

-module(exmpp_server_compression).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    feature/1
]).

%% Compression negotiation.
-export([
    selected_method/1,
    compressed/0,
    failure/1
]).


%%
-export_type([
    method/0,
    methods/0
]).

-type(method() :: binary()).
-type(methods() :: [Method::exmpp_client_compression:method(),...]).

-export_type([
    error_condition/0,
    standard_condition/0,
    standard_conditions/0
]).

-type(error_condition() :: binary()).
-type(standard_condition()
  :: {Error_Condition::exmpp_server_compression:error_condition()}
).

-type(standard_conditions() :: [exmpp_server_compression:standard_condition(),...]).

-export_type([
    xmlel_compression/0,
    xmlel_method/0,
    xmlel_compressed/0,
    xmlel_failure/0
]).

-type(xmlel_compression()
  :: #xmlel{
         name     :: <<_:88>>,
         attrs    :: [],
         children :: [Xmlel_Method::exmpp_server_compression:xmlel_method(),...]
     }
).

-type(xmlel_method()
  :: #xmlel{
         name     :: <<_:48>>,
         attrs    :: [],
         children :: [{'cdata', Method::exmpp_client_compression:method()},...]
     }
).

-type(xmlel_compressed()
  :: #xmlel{
         name     :: <<_:80>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_COMPRESS:: <<_:280>>},...],
         children :: []
     }
).

-type(xmlel_failure()
  :: #xmlel{
         name     :: <<_:56>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_COMPRESS:: <<_:280>>},...],
         children :: []
     }
).


%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

-define(Xmlel@Compress(Name, Attrs, Children),
(
    exxml:element(?NS_COMPRESS, Name, Attrs, Children)
)).

-define(Xmlel@Compress_Feat(Name, Attrs, Children),
(
    exxml:element(?NS_COMPRESS_FEAT, Name, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @throws {stream_compression, feature_announcement, invalid_methods_list,
%%           []} |
%%         {stream_compression, feature_announcement, invalid_method, Method}
%% @doc Make a feature annoucement child.
%%
%% The `Methods' list must contain at least one method.
%%
%% Examples of methods are:
%% <ul>
%% <li>`"zlib"' (support required)</li>
%% <li>`"lzw"'</li>
%% </ul>
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

-spec(feature/1 ::
(
  Methods::exmpp_server_compression:methods())
    -> Xmlel_Compression::exmpp_server_compression:xmlel_compression()
).

feature(Methods) ->
    ?Xmlel@Compress_Feat(<<"compression">>, [], methods_list(Methods)).

%%
-spec(methods_list/1 ::
(
  Methods::exmpp_server_compression:methods())
    -> Xmlels_Method :: [Xmlel_Method::exmpp_server_compression:xmlel_method(),...]
).

methods_list([]) ->
    throw({stream_compression, feature_announcement, invalid_methods_list, []});
methods_list(Methods) ->
    methods_list2(Methods, []).

methods_list2([Method | Methods], Xmlels_Method) ->
    methods_list2(Methods,
        [?Xmlel(<<"method">>, [], [exxml:cdata(Method)]) | Xmlels_Method]);
methods_list2([], Xmlels_Method) ->
    Xmlels_Method.

%% --------------------------------------------------------------------
%% Compression negotiation.
%% --------------------------------------------------------------------
-spec(standard_conditions/0 :: () -> exmpp_server_compression:standard_conditions()).

standard_conditions() ->
    [
     {<<"unsupported-method">>},
     {<<"setup-failed">>}
    ].

%% @doc Extract the method chosen by the initiating entity.
-spec(selected_method/1 ::
(
  Xmlel_Compress::exmpp_client_compression:xmlel_compress())
    -> Method :: exmpp_client_compression:method() | undefined
).

selected_method(Xmlel_Compress) when Xmlel_Compress#xmlel.name == <<"compress">> ->
    case exxml:get_element(Xmlel_Compress, <<"method">>) of
        undefined    -> undefined;
        Xmlel_Method -> exxml:get_cdata(Xmlel_Method)
    end;
selected_method(Xmlel) ->
    throw({stream_compression, selected_method, unexpected_element, Xmlel}).


%% @doc Prepare a `<compressed/>' element.
-spec(compressed/0
  :: () -> Xmlel_Compressed::exmpp_server_compression:xmlel_compressed()
).

compressed() ->
    ?Xmlel@Compress(<<"compressed">>, [], []).

%% @throws {stream_compression, failure, invalid_condition, Condition}
%% @doc Prepare a `<failure/>' element.
-spec(failure/1 ::
(
  Error_Condition::exmpp_server_compression:error_condition())
    -> Xmlel_Failure::exmpp_server_compression:xmlel_failure()
).

failure(Error_Condition) ->
    case lists:keymember(Error_Condition, 1, standard_conditions()) of
        true ->
            ?Xmlel@Compress(<<"failure">>, [], [
                ?Xmlel(Error_Condition, [], [])
            ]);
        false ->
            throw({stream_compression, failure, invalid_condition, Error_Condition})
    end.
