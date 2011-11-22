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

-type(streamversion() :: {non_neg_integer(), non_neg_integer()}).

%% --------------------------------------------------------------------
%% Stream opening/closing.
%% --------------------------------------------------------------------

%% @spec (To, Default_NS, Version) -> Opening
%%     To = binary() |  undefined
%%     Default_NS = binary() 
%%     Version = binary() | | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Opening = exml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% @see opening/4.

-spec opening
(binary() | undefined, binary(), binary() | streamversion()) -> exml:xmlel().

opening(To, Default_NS, Version) ->
    opening(To, Default_NS, Version, undefined).

%% @spec (To, Default_NS, Version, Lang) -> Opening
%%     To = binary() | undefined
%%     Default_NS = binary()
%%     Version = binary() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Lang = binary() | undefined
%%     Opening = exml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% This element is supposed to be sent by the initiating entity
%% to the receiving entity (for the other way around, see {@link
%% opening_reply/1}).

-spec opening
(binary() | undefined, binary(), binary() | streamversion(), binary() | undefined) ->
    xmlel().

opening(To, Default_NS, Version, Lang) ->
    %% Prepare attributes.
    S1 = set_receiving_entity({xmlel, <<?NS_XMPP_pfx/binary,":stream">>, [{<<"xmlns">>, Default_NS}, {<<"xmlns:", ?NS_XMPP_pfx/binary>>, ?NS_XMPP}], undefined}, To),
    S2 = set_version(S1, Version),
    case Lang of 
	    undefined -> S2;
	    _ -> set_lang(S2, Lang)
    end.


%% @spec (From, Default_NS, Version, ID) -> Opening_Reply
%%     From = binary() | undefined
%%     Default_NS = binary() 
%%     Version = binary() | | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() | | undefined
%%     Opening_Reply = exml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% @see opening_reply/5.

-spec opening_reply
(binary() | undefined, binary(),
 binary() |  streamversion(), binary() | random) ->
    exml:xmlel().

opening_reply(From, Default_NS, Version, ID) ->
    opening_reply(From, Default_NS, Version, ID, undefined).

%% @spec (From, Default_NS, Version, ID, Lang) -> Opening_Reply
%%     From = binary() | undefined
%%     Default_NS = binary()
%%     Version = binary() |  {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() |  random
%%     Lang = binary() | undefined
%%     Opening_Reply = exml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(binary() |  undefined, binary(),
 binary() |  streamversion(), binary() | random,
 binary() |  undefined) ->
    exml:xmlel().

opening_reply(From, Default_NS, Version, ID, Lang) ->
    %% Prepare attributes.
    %% Prepare attributes.
    S1 = set_initiating_entity({xmlel, <<?NS_XMPP_pfx/binary,":stream">>, 
		    [{<<"xmlns">>, Default_NS}, {<<"xmlns:",?NS_XMPP_pfx/binary>>, ?NS_XMPP}], undefined}, From),
    S2 = set_id(set_version(S1, Version), ID),
    case Lang of 
	    undefined -> S2;
	    _ -> set_lang(S2, Lang)
    end.

%% @spec (Opening, ID) -> Opening_Reply
%%     Opening = exml:xmlel()
%%     ID = binary() | random
%%     Opening_Reply = exml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(exml:xmlel(), binary() | random) -> exml:xmlel().

opening_reply(Opening, ID) ->
	set_id(exmpp_stanza:reply(Opening), ID).

%% @spec (Opening, ID, Lang) -> Opening_Reply
%%     Opening = exml:xmlel()
%%     ID = binary() | random
%%     Lang = binary() |  undefined
%%     Opening_Reply = exml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(exml:xmlel(), binary() | random, binary() | undefined) ->
    exml:xmlel().

opening_reply(Opening, ID, Lang) ->
	R = opening_reply(Opening, ID),
    	case Lang of
		 undefined -> R;
		 _         -> set_lang(R, Lang)
	     end.

%% @spec () -> Closing
%%     Closing = exml:xmlendtag()
%% @doc Make a `</stream>' closing tag.

-spec closing
() -> exml:xmlendtag().

closing() ->
	{xmlelend, <<?NS_XMPP_pfx/binary, ":stream">>}.

%% @spec (Opening) -> Closing
%%     Opening = exml:xmlel()
%%     Closing = exml:xmlendtag()
%% @doc Make a `</stream>' closing tag for the given `Opening' tag.

-spec closing
(exml:xmlel()) -> exml:xmlendtag().

closing({xmlel, Name, _, _}) ->
	{xmlelend, Name}.

%% --------------------------------------------------------------------
%% Stream standard attributes.
%% --------------------------------------------------------------------

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exml:xmlel()
%%     Hostname = binary()
%% @doc Return the receiving entity hostname.

-spec get_receiving_entity
(exml:xmlel()) -> binary() | undefined.

get_receiving_entity(Opening) ->
    exmpp_stanza:get_recipient(Opening).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exml:xmlel()
%%     Hostname = binary() | string()
%%     New_Opening = exml:xmlel()
%% @doc Set the receiving entity in the `to' attribute.

-spec set_receiving_entity
(exml:xmlel(), binary() ) -> exml:xmlel().

set_receiving_entity(Opening, undefined) ->
	Opening;
set_receiving_entity(Opening, Hostname) ->
 	exmpp_stanza:set_recipient(Opening, Hostname).

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exml:xmlel()
%%     Hostname = binary()
%% @doc Return the initiating entity hostname.

-spec get_initiating_entity
(exml:xmlel()) -> binary() | undefined.

get_initiating_entity(Opening) ->
    exmpp_stanza:get_sender(Opening).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exml:xmlel()
%%     Hostname = binary() 
%%     New_Opening = exml:xmlel()
%% @doc Set the initiating entity in the `from' attribute.

-spec set_initiating_entity
(exml:xmlel(), binary() ) -> exml:xmlel().

set_initiating_entity(El, undefined) ->
	El;
set_initiating_entity(El, Hostname) ->
	exmpp_stanza:set_sender(El, Hostname).

%% @spec (Opening) -> Default_NS | undefined
%%     Opening = exml:xmlel()
%%     Default_NS = atom() | string()
%% @doc Return the default namespace.
%%
%% XMPP-IM defines `jabber:client' and `jabber:server'.

-spec get_default_ns
(exml:xmlel()) -> binary() | undefined.

get_default_ns(Opening) ->
	exml:get_attribute(Opening, <<"xmlns">>).


%% @spec (Opening) -> Version
%%     Opening = exml:xmlel()
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Return the version of the stream.

-spec get_version
(exml:xmlel()) -> streamversion().

get_version(Opening) ->
    parse_version(exml:get_attribute(Opening, <<"version">>)).

%% @spec (Opening, Version) -> New_Opening
%%     Opening = exml:xmlel()
%%     Version = binary() | string() | {Major, Minor} | undefined
%%     Major = integer()
%%     Minor = integer()
%%     New_Opening = exml:xmlel()
%% @doc Set the protocol version.

-spec set_version
(xmlel(), binary() | string() | streamversion()) -> xmlel().

set_version(El, V) when V == undefined; V == <<>>; V == {0,0} ->
	exml:remove_attribute(El, <<"version">>);
set_version(El, {_,_} = V) ->
	set_version(El, serialize_version(V));
set_version(El, V) when is_binary(V)->
	exml:set_attribute(El, <<"version">>, V).


%% @spec (Opening) -> ID | undefined
%%     Opening = exml:xmlel()
%%     ID = binary()
%% @doc Return the stream ID.

-spec get_id
(exml:xmlel()) -> binary() | undefined.

get_id(Opening) ->
    exmpp_stanza:get_id(Opening).

%% @spec (Opening, ID) -> New_Opening
%%     Opening = exml:xmlel()
%%     ID = binary() | string() | random
%%     New_Opening = exml:xmlel()
%% @doc Set the stream ID.

-spec set_id
(xmlel(), binary() | string() | random) -> xmlel().

set_id(El, ID) ->
   exmpp_stanza:set_id(El, ID). 

%% @spec (Opening) -> Lang | undefined
%%     Opening = exml:xmlel()
%%     Lang = binary()
%% @doc Return the language of the stream.

-spec get_lang
(xmlel()) -> binary() | undefined.

get_lang(Opening) ->
    exmpp_stanza:get_lang(Opening).

%% @spec (Opening, Lang) -> New_Opening
%%     Opening = exml:xmlel()
%%     Lang = binary() | string()
%%     New_Opening = exml:xmlel()
%% @doc Set the default language.

-spec set_lang
(xmlel(), binary() | string()) -> xmlel().

set_lang(El, Lang) ->
	exmpp_stanza:set_lang(El, Lang).

%% --------------------------------------------------------------------
%% Version handling.
%% --------------------------------------------------------------------

%% @spec (String) -> Version
%%     String = binary() | string() | undefined
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Parse the stream version in `String'.

-spec parse_version
(binary() | undefined) -> streamversion().

parse_version(undefined) ->
    {0, 0};
parse_version(<<>>) ->
    {0, 0};
parse_version(String) when is_binary(String) ->
    parse_version(binary_to_list(String));
parse_version(String) ->
    case string:to_integer(String) of
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

-spec serialize_version
(streamversion() | undefined) -> binary().

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
%%     Opening = exml:xmlel()
%%     New_Opening = exml:xmlel()
%% @doc Declare server diablack support.

-spec set_dialback_support
(exml:xmlel()) -> exml:xmlel().

set_dialback_support(Opening) ->
	exml:set_attribute(Opening, <<"xmlns:", ?NS_DIALBACK_pfx/binary>>, ?NS_DIALBACK).

%% @spec (Features) -> Features_Announcement
%%     Features = [exml:xmlel()]
%%     Features_Announcement = exml:xmlel()
%% @doc Make the features annoucement element.

-spec features
([exml:xmlel()]) -> exml:xmlel().

features(Features) ->
	{xmlel, <<?NS_XMPP_pfx/binary, ":features">>, [{<<"xmlns:", ?NS_XMPP_pfx/binary>>, ?NS_XMPP}], Features}.

%% --------------------------------------------------------------------
%% Stream-level errors.
%% --------------------------------------------------------------------

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
%%     Stream_Error = exml:xmlel()
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition'.

-spec error
(binary()) -> exml:xmlel().

error(Condition) ->
    error(Condition, {undefined, undefined}).

%% @spec (Condition, {Lang, Text}) -> Stream_Error
%%     Condition = binary()
%%     Stream_Error = exml:xmlel()
%%     Lang = binary() |  undefined
%%     Text = binary() |  undefined
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition' with Text child element.

-spec error
(binary(), {binary() | undefined, binary()| undefined}) ->
    exml:xmlel().

error(Condition, {Lang, Text}) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({stream_error, condition, invalid, Condition})
    end,
    Condition_El = {xmlel, Condition, [{<<"xmlns">>, ?NS_STREAM_ERRORS}], []},
    Error_El0 = {xmlel, <<?NS_XMPP_pfx/binary,":error">>, [],[Condition_El]},

    case Text of
        undefined ->
            Error_El0;
        _ ->
	    Text_El0 = {xmlel, <<"text">>, [{<<"xmlns">>, ?NS_STREAM_ERRORS}],[{cdata, Text}]},
            Text_El = case Lang of
			  undefined ->
			      Text_El0;
			  _ ->
			      exml:set_attribute(Text_El0, <<"lang">>, Lang)
		      end,
            exml:append_child(Error_El0, Text_El)
    end.

%% @spec (XML_El) -> bool()
%%     XML_El = exml:xmlel()
%% @doc Tell if this element is a stream error.

-spec is_error
(exml:xmlel()) -> boolean().

is_error({xmlel, Name, _, _}) when Name == <<"error">> ; Name == <<"stream:error">> ->
    true;
is_error(_) ->
    false.

%% @spec (Stream_Error) -> Condition | undefined
%%     Stream_Error = exml:xmlel()
%%     Condition = atom()
%% @doc Return the child element name corresponding to the stanza error
%% condition.

-spec get_condition
(exml:xmlel()) -> binary() | undefined.

get_condition({xmlel, Name, _, _} = El) 
	when Name == <<"error">> ; Name == <<"stream:error">> -> 
    case exml:get_elements(El) of
	    [{xmlel, Condition, _, _} | _] ->
		    Condition;
	    _ ->
	    %% This <stream:error/> element is invalid because the
	    %% condition must be present (and first).
		    undefined
	end.

%% @spec (Stream_Error) -> Text | undefined
%%     Stream_Error = exml:xmlel()
%%     Text = binary()
%% @doc Return the text that describes the error.

-spec get_text
(exml:xmlel()) -> binary() | undefined.

get_text({xmlel, Name, _, _} = El) 
	when Name == <<"error">> ; Name == <<"stream:error">> -> 
    case exml:get_element(El,  <<"text">>) of
        undefined -> undefined;
        Text      -> exml:get_cdata(Text)
    end.

