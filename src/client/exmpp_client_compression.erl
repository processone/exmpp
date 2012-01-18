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
%% entity side of Stream Compression (XEP-0138).
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0138.html">XEP-0138: Stream Compression</a>
%% @reference <a href="http://www.xmpp.org/extensions/xep-0229.html">XEP-0229: Stream Compression with LZW</a>

-module(exmpp_client_compression).

-include("exmpp.hrl").

%% Feature announcement.
-export([
    announced_methods/1
]).

%% Compression negotiation.
-export([
    selected_method/1
]).

%%
-export_type([
    xmlel_compress/0
]).


-type(xmlel_compress()
  :: #xmlel{
         name     :: <<_:64>>,
         attrs    :: [{XmlNS :: <<_:40>>, NS_COMPRESS:: <<_:280>>},...],
         children :: [{'cdata', Method::exmpp_server_compression:method()},...]
     }
).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @throws {stream_compression, announced_methods, invalid_feature, Feature} |
%%         {stream_compression, announced_methods, invalid_method, El}
%% @doc Return the list of supported compression methods.
-spec(announced_methods/1 ::
(
  Xmlels_Features::exmpp_stream:xmlel_features())
    -> Methods::exmpp_server_compression:methods() | []
).

announced_methods(Xmlel_Features)
  when   Xmlel_Features#xmlel.name == <<"features">>
  orelse Xmlel_Features#xmlel.name == <<"stream:features">> ->
    case exxml:get_element(Xmlel_Features, <<"compression">>) of
        undefined         -> [];
        Xmlel_Compression -> announced_methods2(Xmlel_Compression)
    end.

-spec(announced_methods2/1 ::
(
  Xmlels_Compression::exmpp_server_compression:xmlel_compression())
    -> Methods::exmpp_server_compression:methods()
).

announced_methods2(Xmlel_Compression)
  when Xmlel_Compression#xmlel.children == [] ->
    throw({stream_compression, announced_methods, invalid_feature, Xmlel_Compression});
announced_methods2(#xmlel{children = Children}) ->
    announced_methods3(Children, []).

-spec(announced_methods3/2 ::
(
  Xmlels_Method :: [Xmlel_Method::exmpp_server_compression:xmlel_method(),...]
                 | [],
  Methods       :: [] | exmpp_server_compression:methods())
    -> Methods::exmpp_server_compression:methods()
).

announced_methods3([Xmlel_Method | Xmlels], Methods)
  when Xmlel_Method#xmlel.name == <<"method">> ->
    case exxml:get_cdata(Xmlel_Method) of
        <<>> ->
            throw({stream_compression, announced_methods, invalid_method, Xmlel_Method});
        Method ->
            announced_methods3(Xmlels, [Method | Methods])
    end;
announced_methods3([Xmlel | _Xmlels], _Methods) ->
    throw({stream_compression, announced_methods, invalid_method, Xmlel});
announced_methods3([], Methods) ->
    lists:reverse(Methods).

%% --------------------------------------------------------------------
%% Compression negotiation.
%% --------------------------------------------------------------------

%% @doc Prepare an request to select prefered compression method.
-spec(selected_method/1 ::
(
  Method::exmpp_server_compression:method())
    -> Xmlel_Compress::exmpp_client_compression:xmlel_compress()
).

selected_method(Method) ->
    exxml:element(?NS_COMPRESS, <<"compress">>, [], [
        exxml:element(undefined, <<"method">>, [], [exxml:cdata(Method)])
    ]).
