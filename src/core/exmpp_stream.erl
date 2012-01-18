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
%% The module <strong>{@module}</strong> provides utilities to open and
%% close an XMPP stream, negotiate features and handle stream errors.
%%
%% {@link exmpp_client_stream} and {@link exmpp_server_stream} should be
%% prefered to {@module} because they'll set some defaults values for
%% the caller.
%%
%% <h3>Stream handling</h3>
%%
%% <p>
%% It covers these basic functions:
%% </p>
%% <ul>
%% <li>Open a stream to an XMPP server</li>
%% <li>Open a stream in reply to initiating entity</li>
%% <li>Close a stream (regardless who has initiated the stream)</li>
%% </ul>
%%
%% <p>
%% This table illustrates stream opening and closing.
%% </p>
%% <table class="illustration">
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client call `{@module}':
%% </p>
%% <pre>Opening = exmpp_stream:opening(
%%   "jabber.example.com",
%%   ?NS_JABBER_CLIENT,
%%   "1.0"<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;stream:stream xmlns:stream="http://etherx.jabber.org/streams"
%%   xmlns="jabber:client" to="jabber.example.org" version="1.0"&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% If the server accepts the client stream opening, it'll call:
%% </p>
%% <pre>Opening_Reply = exmpp_stream:opening_reply(
%%   Opening,
%%   random<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;stream:stream xmlns:stream="http://etherx.jabber.org/streams"
%%   xmlns="jabber:client" version="1.0" from="jabber.example.org"
%%   id="stream-396429316"&gt;</pre>
%% <p>
%% Note that `{@module}' generated an ID automatically; you may override
%% this.
%% </p>
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% At the end of the communication, the client close its stream:
%% </p>
%% <pre>Client_Closing = exmpp_stream:closing().</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server do the same:
%% </p>
%% <pre>Server_Closing = exmpp_stream:closing(Client_Closing).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream&gt;</pre>
%% <p>
%% The server may use the same function clause than the client but here,
%% it gives the client closing to the function. This is to be sure to
%% use the same XML prefix.
%% </p>
%% </td>
%% </tr>
%% </table>

-module(exmpp_stream).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Creating elements.
-export([
    opening/3,
    opening/4,
    opening_reply/4,
    opening_reply/5,
    opening_reply/2,
    opening_reply/3,
    closing/0,
    closing/1
]).

%% Attributes handling.
-export([
    get_receiving_entity/1,
    set_receiving_entity/2,
    get_initiating_entity/1,
    set_initiating_entity/2,
    get_default_ns/1,
    get_version/1,
    set_version/2,
    get_id/1,
    set_id/2,
    get_lang/1,
    set_lang/2
]).

%% Version handling.
-export([
    parse_version/1,
    serialize_version/1
]).

%% Features announcement.
-export([
    set_dialback_support/1,
    features/1
]).

%% Error handling.
-export([
    error/1,
    error/2,
    is_error/1,
    get_condition/1,
    get_text/1
]).


%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

%%
-export_type([
  id/0,
  to/0,
  from/0,
  lang/0,
  version/0,
  streamversion/0
]).

-type(id()      :: binary()).
-type(from()    :: binary()).
-type(to()      :: binary()).
-type(lang()    :: binary()).
-type(version() :: binary()).
-type(streamversion() :: {non_neg_integer(), non_neg_integer()}).

-export_type([
  xmlel_stream/0,
  xmlel_features/0,
  xmlel_error/0
]).

-type(xmlel_stream()
  :: #xmlel{
         name     :: <<_:48>> | <<_:104>>,
         attrs    :: [
            {From    :: <<_:32>>, exmpp_stream:from()}    |
            {To      :: <<_:16>>, exmpp_stream:to()}      |
            {Id      :: <<_:16>>, exmpp_stream:id()}      |
            {Version :: <<_:48>>, exmpp_stream:version()} ,...
         ],
         children :: [] | [Xmlel_Error::exmpp_stream:xmlel_error(),...]
     }
).

-type(xmlel_features()
  :: #xmlel{
         name     :: <<_:64>> | <<_:120>>,
         attrs    :: [],
         children :: [exxml:el()]
     }
).

-type(xmlel_error()
  :: #xmlel{
         name     :: <<_:40>> | <<_:96>>,
         attrs    :: [],
         children :: [
             #xmlel{
                 name     :: exmpp_stream:error_condition(),
                 attrs    :: [
                     {XmlNS :: <<_:40>>, <<_:280>>}
                 ],
                 children :: []
             },...
         ]
     }
).

%%

-export_type([
  standard_condition/0,
  standard_conditions/0,
  error_condition/0,
  error_type/0,
  error_text/0,
  error_lang/0
]).

-type(error_condition() :: binary()).
-type(error_type()      :: binary()).
-type(error_lang()      :: binary()).
-type(error_text()      :: binary()).

-type(standard_condition()
  :: {Error_Condition :: exmpp_stream:error_condition()}

).

-type(standard_conditions()
  :: [Standard_Condition::exmpp_stream:standard_condition(),...]
).

%%

-define(Xmlel@Stream(Name, Attrs, Children),
(
    exxml:element(undefined,
        <<?NS_XMPP_pfx/binary, <<":">>/binary, Name/binary>>,
        Attrs,
        Children)
%    #xmlel{
%        name     = <<?NS_XMPP_pfx/binary, <<":">>/binary, Name/binary>>,
%        attrs    = Attrs,
%        children = Children
%    }
)).

-define(Xmlel_Stream@Stream(Attrs, Children),
(
    ?Xmlel@Stream(<<"stream">>, Attrs, Children)
)).

-define(Xmlel_Features@Stream(Attrs, Children),
(
    ?Xmlel@Stream(<<"features">>, Attrs, Children)
)).

%% --------------------------------------------------------------------
%% Stream opening/closing.
%% --------------------------------------------------------------------

%% @spec (To, Default_NS, Version) -> Opening
%%     To = binary() |  undefined
%%     Default_NS = binary() 
%%     Version = binary() | | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Opening = exxml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% @see opening/4.

-spec(opening/3 ::
(
  To         :: exmpp_stream:to() | undefined,
  Default_NS :: binary(),
  Version    :: exmpp_stream:version() | exmpp_stream:streamversion())
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening(To, Default_NS, Version) ->
    opening(To, Default_NS, Version, undefined).

%% @spec (To, Default_NS, Version, Lang) -> Opening
%%     To = binary() | undefined
%%     Default_NS = binary()
%%     Version = binary() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Lang = binary() | undefined
%%     Opening = exxml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% This element is supposed to be sent by the initiating entity
%% to the receiving entity (for the other way around, see {@link
%% opening_reply/1}).

-spec(opening/4 ::
(
  To         :: exmpp_stream:to() | undefined,
  Default_NS :: binary(),
  Version    :: exmpp_stream:version() | exmpp_stream:streamversion(),
  Lang       :: exmpp_stream:lang() | undefined)
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening(To, Default_NS, Version, Lang) ->
    Xmlel_Stream = set_version(
        set_receiving_entity(
            ?Xmlel_Stream@Stream(
                [{<<"xmlns">>, Default_NS},
                 {<<"xmlns:", ?NS_XMPP_pfx/binary>>, ?NS_XMPP}], undefined),
            To),
        Version),
    case Lang of
        undefined -> Xmlel_Stream;
        _         -> set_lang(Xmlel_Stream, Lang)
    end.

%% @spec (From, Default_NS, Version, ID) -> Opening_Reply
%%     From = binary() | undefined
%%     Default_NS = binary() 
%%     Version = binary() | | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() | | undefined
%%     Opening_Reply = exxml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% @see opening_reply/5.

-spec(opening_reply/4 ::
(
  From       :: exmpp_stream:from() | undefined,
  Default_NS :: binary(),
  Version    :: exmpp_stream:streamversion() | exmpp_stream:version(),
  Id         :: exmpp_stream:id() | 'random')
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening_reply(From, Default_NS, Version, Id) ->
    opening_reply(From, Default_NS, Version, Id, undefined).

%% @spec (From, Default_NS, Version, ID, Lang) -> Opening_Reply
%%     From = binary() | undefined
%%     Default_NS = binary()
%%     Version = binary() |  {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() |  random
%%     Lang = binary() | undefined
%%     Opening_Reply = exxml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec(opening_reply/5 ::
(
  From       :: exmpp_stream:from() | undefined,
  Default_NS :: binary(),
  Version    :: exmpp_stream:streamversion() | exmpp_stream:version(),
  Id         :: exmpp_stream:id() | 'random',
  Lang       :: exmpp_stream:lang() | undefined)
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening_reply(From, Default_NS, Version, Id, Lang) ->
    Xmlel_Stream = set_id(
        set_version(
            set_initiating_entity(
                ?Xmlel_Stream@Stream(
                    [{<<"xmlns">>, Default_NS},
                     {<<"xmlns:", ?NS_XMPP_pfx/binary>>, ?NS_XMPP}], undefined),
                From),
            Version),
        Id),
    case Lang of
        undefined -> Xmlel_Stream;
        _         -> set_lang(Xmlel_Stream, Lang)
    end.

%% @spec (Opening, ID) -> Opening_Reply
%%     Opening = exxml:xmlel()
%%     ID = binary() | random
%%     Opening_Reply = exxml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec(opening_reply/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Id           :: exmpp_stream:id() | 'random')
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening_reply(Opening, Id) ->
    set_id(exmpp_stanza:reply(Opening), Id).

%% @spec (Opening, ID, Lang) -> Opening_Reply
%%     Opening = exxml:xmlel()
%%     ID = binary() | random
%%     Lang = binary() |  undefined
%%     Opening_Reply = exxml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec(opening_reply/3 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Id           :: exmpp_stream:id() | 'random',
  Lang         :: exmpp_stream:lang() | undefined)
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

opening_reply(Xmlel_Stream, Id, undefined = _Lang) ->
    opening_reply(Xmlel_Stream, Id);
opening_reply(Xmlel_Stream, Id, Lang) ->
    set_lang(opening_reply(Xmlel_Stream, Id), Lang).

%% @spec () -> Closing
%%     Closing = exxml:xmlendtag()
%% @doc Make a `</stream>' closing tag.

-spec(closing/0 :: () -> Xml_End_Tag::exxml:endtag()).

closing() ->
    {xmlelend, <<?NS_XMPP_pfx/binary, ":stream">>}.

%% @spec (Opening) -> Closing
%%     Opening = exxml:xmlel()
%%     Closing = exxml:xmlendtag()
%% @doc Make a `</stream>' closing tag for the given `Opening' tag.

-spec(closing/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Xml_End_Tag::exxml:endtag()
).

closing(#xmlel{name = Name}) ->
    {xmlelend, Name}.

%% --------------------------------------------------------------------
%% Stream standard attributes.
%% --------------------------------------------------------------------

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exxml:xmlel()
%%     Hostname = binary()
%% @doc Return the receiving entity hostname.

-spec(get_receiving_entity/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Hostname::exmpp_stream:to() | undefined
).

get_receiving_entity(Xmlel_Stream) ->
    exmpp_stanza:get_recipient(Xmlel_Stream).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exxml:xmlel()
%%     Hostname = binary() | string()
%%     New_Opening = exxml:xmlel()
%% @doc Set the receiving entity in the `to' attribute.

-spec(set_receiving_entity/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Hostname     :: exmpp_stream:to() | undefined)
    -> Xmlel_Stream :: exmpp_stream:xmlel_stream()
).

set_receiving_entity(Xmlel_Stream, undefined) ->
    Xmlel_Stream;
set_receiving_entity(Xmlel_Stream, Hostname) ->
    exmpp_stanza:set_recipient(Xmlel_Stream, Hostname).

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exxml:xmlel()
%%     Hostname = binary()
%% @doc Return the initiating entity hostname.

-spec(get_initiating_entity/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Hostname::exmpp_stream:from() | undefined
).

get_initiating_entity(Xmlel_Stream) ->
    exmpp_stanza:get_sender(Xmlel_Stream).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exxml:xmlel()
%%     Hostname = binary() 
%%     New_Opening = exxml:xmlel()
%% @doc Set the initiating entity in the `from' attribute.

-spec(set_initiating_entity/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Hostname     :: exmpp_stream:from() | undefined)
    -> Xmlel_Stream :: exmpp_stream:xmlel_stream()
).

set_initiating_entity(Xmlel_Stream, undefined) ->
    Xmlel_Stream;
set_initiating_entity(Xmlel_Stream, Hostname) ->
    exmpp_stanza:set_sender(Xmlel_Stream, Hostname).

%% @spec (Opening) -> Default_NS | undefined
%%     Opening = exxml:xmlel()
%%     Default_NS = atom() | string()
%% @doc Return the default namespace.
%%
%% XMPP-IM defines `jabber:client' and `jabber:server'.

-spec(get_default_ns/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Default_NS :: binary() | undefined
).

get_default_ns(Xmlel_Stream) ->
    exxml:get_attribute(Xmlel_Stream, <<"xmlns">>).


%% @spec (Opening) -> Version
%%     Opening = exxml:xmlel()
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Return the version of the stream.

-spec(get_version/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Stream_Version::exmpp_stream:streamversion()
).

get_version(Xmlel_Stream) ->
    parse_version(exxml:get_attribute(Xmlel_Stream, <<"version">>)).

%% @spec (Opening, Version) -> New_Opening
%%     Opening = exxml:xmlel()
%%     Version = binary() | string() | {Major, Minor} | undefined
%%     Major = integer()
%%     Minor = integer()
%%     New_Opening = exxml:xmlel()
%% @doc Set the protocol version.

-spec(set_version/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Version      :: exmpp_stream:streamversion()
                | exmpp_stream:version()
                | undefined)
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

set_version(Xmlel_Stream, Version)
  when Version == undefined; Version == <<>>; Version == {0,0} ->
    exxml:remove_attribute(Xmlel_Stream, <<"version">>);
set_version(Xmlel_Stream, {_,_} = Version) ->
    set_version(Xmlel_Stream, serialize_version(Version));
set_version(Xmlel_Stream, Version) when is_binary(Version)->
    exxml:set_attribute(Xmlel_Stream, <<"version">>, Version).


%% @spec (Opening) -> ID | undefined
%%     Opening = exxml:xmlel()
%%     ID = binary()
%% @doc Return the stream ID.

-spec(get_id/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Id :: exmpp_stream:id() | undefined
).

get_id(Xmlel_Stream) ->
    exmpp_stanza:get_id(Xmlel_Stream).

%% @spec (Opening, ID) -> New_Opening
%%     Opening = exxml:xmlel()
%%     ID = binary() | string() | random
%%     New_Opening = exxml:xmlel()
%% @doc Set the stream ID.

-spec(set_id/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Id           :: exmpp_stream:id() | 'random')
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

set_id(Xmlel_Stream, Id) ->
   exmpp_stanza:set_id(Xmlel_Stream, Id).

%% @spec (Opening) -> Lang | undefined
%%     Opening = exxml:xmlel()
%%     Lang = binary()
%% @doc Return the language of the stream.

-spec(get_lang/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Lang :: exmpp_stream:lang() | undefined
).

get_lang(Xmlel_Stream) ->
    exmpp_stanza:get_lang(Xmlel_Stream).

%% @spec (Opening, Lang) -> New_Opening
%%     Opening = exxml:xmlel()
%%     Lang = binary() | string()
%%     New_Opening = exxml:xmlel()
%% @doc Set the default language.

-spec(set_lang/2 ::
(
  Xmlel_Stream :: exmpp_stream:xmlel_stream(),
  Lang         :: exmpp_stream:lang())
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

set_lang(Xmlel_Stream, Lang) ->
    exmpp_stanza:set_lang(Xmlel_Stream, Lang).

%% --------------------------------------------------------------------
%% Version handling.
%% --------------------------------------------------------------------

%% @spec (String) -> Version
%%     String = binary() | string() | undefined
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Parse the stream version in `String'.

-spec(parse_version/1 ::
(
  Version :: exmpp_stream:version() | string() | undefined)
    -> Stream_Version::exmpp_stream:streamversion()
).

parse_version(undefined) ->
    {0, 0};
parse_version(<<>>) ->
    {0, 0};
parse_version(Version) when is_binary(Version) ->
    parse_version(binary_to_list(Version));
parse_version(Version) ->
    case string:to_integer(Version) of
        {Major, [$. | Rest]} ->
            case string:to_integer(Rest) of
                {Minor, []} -> {Major, Minor};
                _           -> {error, invalid_version}
            end;
        _ ->
            {error, invalid_version}
    end.

%% @spec (Version) -> Binary
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Binary = binary()
%% @doc Make a binary() for the `version' attribute of a stream element.

-spec(serialize_version/1 ::
(
  Stream_Version :: exmpp_stream:streamversion() | undefined)
    -> Version :: exmpp_stream:version()
).

serialize_version(undefined) ->
    <<>>;
serialize_version({0, 0}) ->
    <<>>;
serialize_version({Major, Minor}) ->
    list_to_binary(lists:flatten(io_lib:format("~b.~b", [Major, Minor]))).

%% --------------------------------------------------------------------
%% Features announcement.
%% --------------------------------------------------------------------

%% @spec (Opening) -> New_Opening
%%     Opening = exxml:xmlel()
%%     New_Opening = exxml:xmlel()
%% @doc Declare server diablack support.

-spec(set_dialback_support/1 ::
(
  Xmlel_Stream::exmpp_stream:xmlel_stream())
    -> Xmlel_Stream::exmpp_stream:xmlel_stream()
).

set_dialback_support(Xmlel_Stream) ->
    exxml:set_attribute(Xmlel_Stream,
        <<"xmlns:", ?NS_DIALBACK_pfx/binary>>, ?NS_DIALBACK).

%% @spec (Features) -> Features_Announcement
%%     Features = [exxml:xmlel()]
%%     Features_Announcement = exxml:xmlel()
%% @doc Make the features annoucement element.

-spec(features/1 ::
(
  Features :: [Feature::exxml:el()])
    -> Xmlel_Features::exmpp_stream:xmlel_features()
).

features(Features) ->
    ?Xmlel_Features@Stream([{<<"xmlns:", ?NS_XMPP_pfx/binary>>, ?NS_XMPP}],
        Features).

%% --------------------------------------------------------------------
%% Stream-level errors.
%% --------------------------------------------------------------------

-spec(standard_conditions/0
  :: () -> Standard_Conditions::exmpp_stream:standard_conditions()
).

standard_conditions() ->
    [
     {<<"bad-format">>},
     {<<"bad-namespace-prefix">>},
     {<<"conflict">>},
     {<<"connection-timeout">>},
     {<<"host-gone">>},
     {<<"host-unknown">>},
     {<<"improper-addressing">>},
     {<<"internal-server-error">>},
     {<<"invalid-from">>},
     {<<"invalid-id">>},
     {<<"invalid-namespace">>},
     {<<"invalid-xml">>},
     {<<"not-authorized">>},
     {<<"policy-violation">>},
     {<<"remote-connection-failed">>},
     {<<"resource-constraint">>},
     {<<"restricted-xml">>},
     {<<"see-other-host">>},
     {<<"system-shutdown">>},
     {<<"undefined-condition">>},
     {<<"unsupported-encoding">>},
     {<<"unsupported-stanza-type">>},
     {<<"unsupported-version">>},
     {<<"xml-not-well-formed">>},
     %% rfc3920bis
     {<<"not-well-formed">>},
     {<<"reset">>}
    ].

%% @spec (Condition) -> Stream_Error
%%     Condition = binary()
%%     Stream_Error = exxml:xmlel()
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition'.

-spec(error/1 ::
(
  Error_Condition :: exmpp_stream:error_condition())
    -> Xmlel_Error::exmpp_stream:xmlel_error()
).

error(Error_Condition) ->
    error(Error_Condition, {undefined, undefined}).

%% @spec (Condition, {Lang, Text}) -> Stream_Error
%%     Condition = binary()
%%     Stream_Error = exxml:xmlel()
%%     Lang = binary() |  undefined
%%     Text = binary() |  undefined
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition' with Text child element.

-spec(error/2 ::
(
  Error_Condition :: exmpp_stream:error_condition(),
  {Error_Lang :: exmpp_stream:error_lang() | undefined,
   Error_Text :: exmpp_stream:error_text() | undefined})
    -> Xmlel_Error::exmpp_stream:xmlel_error()
).

error(Error_Condition, {_Error_Lang, undefined = _Error_Text}) ->
    case lists:keymember(Error_Condition, 1, standard_conditions()) of
         true  -> ok;
         false -> throw({stream_error, condition, invalid, Error_Condition})
    end,
    exxml:element(undefined, <<?NS_XMPP_pfx/binary,":error">>, [], [
        exxml:element(undefined, Error_Condition,
            [{<<"xmlns">>, ?NS_STREAM_ERRORS}], [])
    ]);
error(Error_Condition, {Error_Lang, Error_Text}) ->
    case lists:keymember(Error_Condition, 1, standard_conditions()) of
         true  -> ok;
         false -> throw({stream_error, condition, invalid, Error_Condition})
    end,
    exxml:element(undefined, <<?NS_XMPP_pfx/binary,":error">>, [], [
        exxml:element(undefined, Error_Condition,
            [exxml:attribute(<<"xmlns">>, ?NS_STREAM_ERRORS)], []),
        exxml:element(undefined, <<"text">>,
            case Error_Lang of
                undefined ->
                    [{<<"xmlns">>, ?NS_STREAM_ERRORS}];
                _ ->
                    [exxml:attribute(<<"lang">>, Error_Lang),
                     {<<"xmlns">>, ?NS_STREAM_ERRORS}]
            end,
            [exxml:cdata(Error_Text)])
    ]).

%% @spec (XML_El) -> bool()
%%     XML_El = exxml:xmlel()
%% @doc Tell if this element is a stream error.

-spec(is_error/1 ::
(
  Xmlel_Error :: exmpp_stream:xmlel_error() | any())
    -> Is_Error::boolean()
).

is_error(#xmlel{name = Name})
  when Name == <<"error">> ; Name == <<"stream:error">> ->
    true;
is_error(_) ->
    false.

%% @spec (Stream_Error) -> Condition | undefined
%%     Stream_Error = exxml:xmlel()
%%     Condition = atom()
%% @doc Return the child element name corresponding to the stanza error
%% condition.

-spec(get_condition/1 ::
(
  Xmlel_Error::exmpp_stream:xmlel_error())
    -> Error_Condition :: exmpp_stream:error_condition() | undefined
).

get_condition(Xmlel_Error) 
  when Xmlel_Error#xmlel.name == <<"error">> ;
       Xmlel_Error#xmlel.name == <<"stream:error">> -> 
    case exxml:get_elements(Xmlel_Error) of
        [#xmlel{name = Error_Condition} | _] ->
            Error_Condition;
        _ ->
        %% This <stream:error/> element is invalid because the
        %% condition must be present (and first).
            undefined
    end.

%% @spec (Stream_Error) -> Text | undefined
%%     Stream_Error = exxml:xmlel()
%%     Text = binary()
%% @doc Return the text that describes the error.

-spec(get_text/1 ::
(
  Xmlel_Error::exmpp_stream:xmlel_error())
    -> Error_Text :: exmpp_stream:error_text() | undefined
).

get_text(Xmlel_Error)
  when Xmlel_Error#xmlel.name == <<"error">> ;
       Xmlel_Error#xmlel.name == <<"stream:error">> ->
    case exxml:get_element(Xmlel_Error,  <<"text">>) of
        undefined  -> undefined;
        Xmlel_Text -> exxml:get_cdata(Xmlel_Text)
    end.

