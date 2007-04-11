% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides utilities to open and close
%% an XMPP stream, negotiate features and handle stream errors.
%%
%% {@link exmpp_client_stream} and {@link exmpp_server_stream} should be
%% prefered to {@module} because they'll set some defaults values for the
%% caller.
%%
%% <h3>Stream handling</h3>
%%
%% <p>
%% It covers these basic functions:
%% </p>
%% <ul>
%% <li>Open a stream to an XMPP server</li>
%% <li>Open a stream in reply to initiating peer</li>
%% <li>Close a stream (regardless who has initiated the stream)</li>
%% </ul>
%%
%% <p>
%% A common use case is illustrated in <em>table 1</em>.
%% </p>
%% <table class="illustration">
%% <caption>Table 1: stream opening and closing</caption>
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client call `{@module}':
%% </p>
%% <pre>Opening = exmpp_client_stream:stream_opening([
%%   {to, "jabber.example.com"},
%%   {version, "1.0"}<br/>]).</pre>
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
%% <pre>Opening_Reply = exmpp_server_stream:stream_opening_reply(Opening).</pre>
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
%% <pre>Client_Closing = exmpp_client_stream:stream_closing().</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream"&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server do the same:
%% </p>
%% <pre>Server_Closing = exmpp_server_stream:stream_closing(Client_Closing).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream"&gt;</pre>
%% <p>
%% The server may use the same function clause than the client but here,
%% it gives the client closing to the function. This is to be sure to
%% use the same XML prefix.
%% </p>
%% </td>
%% </tr>
%% </table>

-module(exmpp_stream).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
  stream_opening/1,
  stream_opening_reply/1,
  stream_closing/0,
  stream_closing/1,
  stream_error/1
]).

% --------------------------------------------------------------------
% Stream opening/closing.
% --------------------------------------------------------------------

%% @spec (Args) -> Stream_Opening | {error, Reason}
%%     Args = [Arg]
%%     Arg = Context_Spec | To_Spec | Version_Spec | Lang_Spec
%%     Context_Spec = {context, client} | {context, server}
%%     To_Spec = {to, string()}
%%     Version_Spec = {version, string()}
%%     Lang_Spec = {lang, string()}
%%     Stream_Opening = exmpp_xml:xmlnselement()
%% @doc Make a `<stream>' opening tag.
%%
%% This element is supposed to be sent by the initiating peer
%% to the receiving peer (for the other way around, see {@link
%% stream_opening_reply/1}).
%%
%% Only `Context_Spec' is mandatory. It indicates if the stream to be
%% opened is between a client and a server (`client') or between two
%% servers (`server').

stream_opening(Args) ->
    case stream_opening_attributes(Args) of
        {error, Reason} ->
            {error, Reason};
        Attrs ->
            Stream_Opening = #xmlnselement{
              ns     = ?NS_XMPP,
              prefix = "stream",
              name   = stream,
              attrs  = Attrs
            },
            case check_stream_opening(Stream_Opening) of
                ok ->
                    Stream_Opening;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

stream_opening_attributes(Args) ->
    stream_opening_attributes2(Args, []).

stream_opening_attributes2([{to, To} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'to', To),
    stream_opening_attributes2(Rest, New_Attrs);
stream_opening_attributes2([{version, Version} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'version', Version),
    stream_opening_attributes2(Rest, New_Attrs);
stream_opening_attributes2([{lang, Lang} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      ?NS_XML, 'lang', Lang),
    stream_opening_attributes2(Rest, New_Attrs);
stream_opening_attributes2([{context, client} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'xmlns', atom_to_list(?NS_JABBER_CLIENT)),
    stream_opening_attributes2(Rest, New_Attrs);
stream_opening_attributes2([{context, server} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'xmlns', atom_to_list(?NS_JABBER_SERVER)),
    stream_opening_attributes2(Rest, New_Attrs);
stream_opening_attributes2([{context, Bad_Context} | _Rest], _Attrs) ->
    {error, {unknown_context, Bad_Context}};

stream_opening_attributes2([Bad_Arg | _Rest], _Attrs) ->
    {error, {unknown_argument, Bad_Arg}};

stream_opening_attributes2([], Attrs) ->
    Attrs.

check_stream_opening(Stream_Opening) ->
    case exmpp_xml:has_attribute(Stream_Opening, 'xmlns') of
        true ->
            ok;
        false ->
            {error, unspecified_context}
    end.

%% @spec (Stream_Opening | Args) -> Stream_Opening_Reply
%%     Stream_Opening = exmpp_xml:xmlnselement()
%%     Args = [Arg]
%%     Arg = Context_Spec | From_Spec | ID_Spec | Version_Spec | Lang_Spec
%%     Context_Spec = {context, client} | {context, server}
%%     From_Spec = {from, string()}
%%     ID_Spec = {id, string()} | {if, undefined}
%%     Version_Spec = {version, string()}
%%     Lang_Spec = {lang, string()}
%%     Stream_Opening_Reply = exmpp_xml:xmlnselement()
%% @doc Make a `<stream>' opening reply tag.
%%
%% This element is supposed to be sent by the receiving peer in reply
%% to the initiating peer (for the other way around, see {@link
%% stream_opening/1}).
%%
%% Only `Context_Spec' is mandatory (see {@link stream_opening/1} for
%% its meaning).
%%
%% If `ID_Spec' is `{id, undefined}', one will be generated
%% automatically.

stream_opening_reply(#xmlnselement{attrs = Attrs} = Stream_Opening) ->
    Id = stream_id(),
    Attrs1 = exmpp_jlib:rename_attr_to_to_from_in_list(Attrs),
    Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'id', Id),
    Stream_Opening#xmlnselement{attrs = Attrs2};

stream_opening_reply(Args) ->
    case stream_opening_reply_attributes(Args) of
        {error, Reason} ->
            {error, Reason};
        Attrs ->
            Stream_Opening_Reply = #xmlnselement{
              ns     = ?NS_XMPP,
              prefix = "stream",
              name   = stream,
              attrs  = Attrs
            },
            case check_stream_opening_reply(Stream_Opening_Reply) of
                ok ->
                    Stream_Opening_Reply;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

stream_opening_reply_attributes(Args) ->
    stream_opening_reply_attributes2(Args, []).

stream_opening_reply_attributes2([{from, From} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'from', From),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{id, undefined} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'id', stream_id()),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{id, ID} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'id', ID),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{version, Version} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'version', Version),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{lang, Lang} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      ?NS_XML, 'lang', Lang),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{context, client} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'xmlns', atom_to_list(?NS_JABBER_CLIENT)),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{context, server} | Rest], Attrs) ->
    New_Attrs = exmpp_xml:set_attribute_in_list(Attrs,
      'xmlns', atom_to_list(?NS_JABBER_SERVER)),
    stream_opening_reply_attributes2(Rest, New_Attrs);
stream_opening_reply_attributes2([{context, Bad_Context} | _Rest], _Attrs) ->
    {error, {unknown_context, Bad_Context}};

stream_opening_reply_attributes2([Bad_Arg | _Rest], _Attrs) ->
    {error, {unknown_argument, Bad_Arg}};

stream_opening_reply_attributes2([], Attrs) ->
    Attrs.

check_stream_opening_reply(Stream_Opening_Reply) ->
    case exmpp_xml:has_attribute(Stream_Opening_Reply, 'xmlns') of
        true ->
            ok;
        false ->
            {error, unspecified_context}
    end.

%% @spec () -> Stream_Closing
%%     Stream_Closing = exmpp_xml:xmlnsendelement()
%% @doc Make a `</stream>' closing tag.

stream_closing() ->
    #xmlnsendelement{ns = ?NS_XMPP, prefix = "stream", name = 'stream'}.

%% @spec (Stream_Opening) -> Stream_Closing
%%     XML_End_Element = exmpp_xml:xmlnsendelement()
%% @doc Make a `</stream>' closing tag from the given `Stream_Opening' tag.

stream_closing(#xmlnselement{ns = NS, prefix = Prefix, name = Name}) ->
    #xmlnsendelement{ns = NS, prefix = Prefix, name = Name}.

%% @spec () -> Stream_ID
%%     Stream_ID = string()
%% @doc Generate a random stream ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

stream_id() ->
    "stream-" ++ integer_to_list(random:uniform(65536 * 65536)).

% --------------------------------------------------------------------
% Stream-level errors.
% --------------------------------------------------------------------

%% @spec (Type) -> Stream_Error
%%     Type = atom()
%%     Stream_Error = exmpp_xml:xmlnselement()
%% @doc Make a `<stream:error>' element corresponding to the given `Type'.

stream_error(Type) ->
    Type_El = #xmlnselement{
      ns = ?NS_XMPP_STREAMS,
      name = Type,
      children = []
    },
    #xmlnselement{
      ns = ?NS_XMPP,
      prefix = "stream",
      name = 'error',
      children = [Type_El]
    }.
