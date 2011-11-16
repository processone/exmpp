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

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> Methods
%%     Features_Announcement = exml:xmlel()
%%     Methods = [binary()]
%% @throws {stream_compression, announced_methods, invalid_feature, Feature} |
%%         {stream_compression, announced_methods, invalid_method, El}
%% @doc Return the list of supported compression methods.

announced_methods({xmlel, F, _Attrs, _Children} = El) 
	when F == <<"features">> orelse F == <<"stream:features">> ->
    case exml:get_element(El,<<"compression">>) of
        undefined -> [];
        Methods   -> announced_methods2(Methods)
    end.

announced_methods2({xmlel, _, _, []} = Feature) ->
    throw({stream_compression, announced_methods, invalid_feature, Feature});
announced_methods2({xmlel, _, _, Children}) ->
    announced_methods3(Children, []).

announced_methods3( [{xmlel, <<"method">>, _, _} = El | Rest], Result) ->
    case exml:get_cdata(El) of
        <<>> ->
            throw({stream_compression, announced_methods, invalid_method, El});
        Method ->
            announced_methods3(Rest, [Method | Result])
    end;
announced_methods3([El | _Rest], _Result) ->
    throw({stream_compression, announced_methods, invalid_method, El});
announced_methods3([], Result) ->
    lists:reverse(Result).

%% --------------------------------------------------------------------
%% Compression negotiation.
%% --------------------------------------------------------------------

%% @spec (Method) -> Compress
%%     Method = binary()
%%     Compress = exml:xmlel()
%% @doc Prepare an request to select prefered compression method.

selected_method(Method) ->
	{xmlel, <<"compress">>, [{<<"xmlns">>, ?NS_COMPRESS}], 
		[{xmlel, <<"method">>, [], [{cdata, Method}]}]}.
