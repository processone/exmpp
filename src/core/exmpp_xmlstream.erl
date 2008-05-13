% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> sends events to a specified
%% process or function based on elements and trees returned by the
%% parser `exmpp_xml'.
%%
%% <p>
%% It also provides a high-level function to parse an XML document
%% without the overhead of parser initialization.
%% </p>
%%
%% <p><strong>The API isn't stabilized yet</strong>.
%% What's left to be done:</p>
%% <ul>
%% <li>rework API (function names with more sense especially)</li>
%% </ul>

-module(exmpp_xmlstream).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([start/1, start/2, stop/1, parse/2]).
-export([parse_element/1, parse_element/2]).

-record(xml_stream, {
  callback,
  parser,
  opened = 0
}).

% --------------------------------------------------------------------
% Stream parsing, chunk by chunk.
% --------------------------------------------------------------------

%% @spec (Parser_Options | Callback) -> {ok, Stream} | {error, Reason}
%%     Callback = callback()
%%     Parser_Options = [exmpp_xml:xmlparseroption()]
%%     Stream = xmlstream()
%% @doc Start a new stream handler.
%%
%% If the caller specifies a callback, the XML parser is created with
%% default options, but the `root_depth' which is 1 and `endelement'
%% which is set.
%%
%% <br/><br/>
%% If the caller specifies parser options, the callback will be set to
%% `no_callback'.
%%
%% <br/><br/>
%% This function (or {@link start/2}) must be called before any use of
%% {@link parse/2}.
%% @see exmpp_xml:start_parser/0.
%% @see exmpp_xml:xmlparseroption().

start(Parser_Options) when is_list(Parser_Options) ->
    start(no_callback, Parser_Options);

start(Callback) ->
    start(Callback, []).

%% @spec (Callback, Parser_Options) -> {ok, Stream} | {error, Reason}
%%     Callback = callback()
%%     Stream = xmlstream()
%%     Parser_Options = [exmpp_xml:xmlparseroption()]
%% @doc Start a new stream handler.
%%
%% The XML parser is created with `Parser_Options' options (by default,
%% `root_depth' is 1 and `endelement' is set). This function (or {@link
%% start/1}) must be called before any use of {@link parse/2}.
%%
%% @see exmpp_xml:start_parser/1.

start(Callback, Parser_Options) ->
    Parser_Options2 = [{root_depth, 1}, endelement | Parser_Options],
    Parser = exmpp_xml:start_parser(Parser_Options2),
    Callback2 = case Callback of
        Pid when is_pid(Pid) -> {process, Pid};
        _                    -> Callback
    end,
    #xml_stream{
      callback = Callback2,
      parser = Parser
    }.

%% @spec (Stream) -> ok | {error, Reason}
%%     Stream = xmlstream()
%% @doc Close a stream handler.
%%
%% This must be called when `Stream' (returned by {@link start/0} or
%% {@link start/1} isn't necessary anymore. This will terminate the
%% parser.
%%
%% @see start/0.
%% @see start/1.

stop(#xml_stream{parser = Parser} = _Stream) ->
    exmpp_xml:stop_parser(Parser).

%% @spec (Stream, Data) -> {ok, New_Stream} | {ok, New_Stream, Events} | {error, Reason}
%%     Stream = xmlstream()
%%     Data = string() | binary()
%%     New_Stream = xmlstream()
%%     Events = [xmlstreamevent()]
%% @doc Parse a chunk of XML data and send events to the callback
%% process or function, or return them to the caller.
%%
%% If `Data' doesn't contain one or more complete XML elements, this
%% function may not send any event.
%%
%% Potential events are described by the {@link xmlstreamevent()} type.

parse(#xml_stream{parser = Parser} = Stream, Data) ->
    try exmpp_xml:parse(Parser, Data) of
        continue ->
            send_events(Stream, []);
        XML_Elements ->
            {ok, New_Stream, Events} = process_elements(Stream, XML_Elements),
            send_events(New_Stream, Events)
    catch
        throw:{xml_parser, parsing, malformed_xml, Reason} ->
            send_events(Stream, [{xmlstreamerror, Reason}]);
        throw:Exception ->
            throw(Exception)
    end.

process_elements(Stream, XML_Elements) ->
    process_elements2(Stream, XML_Elements, []).

process_elements2(Stream, [XML_Element | Rest], Events) ->
    case XML_Element of
        % With namespace support.
        #xmlnselement{} when Stream#xml_stream.opened == 0 ->
            % Stream is freshly opened.
            New_Stream = Stream#xml_stream{opened = 1},
            New_Events = [#xmlstreamstart{element = XML_Element} |
              Events],
            process_elements2(New_Stream, Rest, New_Events);
        #xmlnselement{} ->
            % An "depth 1" element and its children.
            New_Events = [#xmlstreamelement{element = XML_Element} |
              Events],
            process_elements2(Stream, Rest, New_Events);
        #xmlnsendelement{} ->
            % Stream is closed.
            New_Stream = Stream#xml_stream{opened = 0},
            New_Events = [#xmlstreamend{endelement = XML_Element} |
              Events],
            process_elements2(New_Stream, Rest, New_Events);

        % Without namespace support.
        #xmlelement{} when Stream#xml_stream.opened == 0 ->
            % Stream is freshly opened.
            New_Stream = Stream#xml_stream{opened = 1},
            New_Events = [#xmlstreamstart{element = XML_Element} |
              Events],
            process_elements2(New_Stream, Rest, New_Events);
        #xmlelement{} ->
            % An "depth 1" element and its children.
            New_Events = [#xmlstreamelement{element = XML_Element} |
              Events],
            process_elements2(Stream, Rest, New_Events);
        #xmlendelement{} ->
            % Stream is closed.
            New_Stream = Stream#xml_stream{opened = 0},
            New_Events = [#xmlstreamend{endelement = XML_Element} |
              Events],
            process_elements2(New_Stream, Rest, New_Events)

        % Unknown tuple.
        %_ ->
        %    error_logger:info_msg(
        %      "~s:process_elements/2: Unknown element: ~p~n",
        %      [?MODULE, XML_Element]),
        %    process_elements(Stream, Rest)
    end;
process_elements2(Stream, [], Events) ->
    {ok, Stream, lists:reverse(Events)}.

send_events(#xml_stream{callback = {gen_fsm, Pid}} = Stream,
  [Event | Rest]) ->
    case catch gen_fsm:send_event(Pid, Event) of
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        ok ->
            send_events(Stream, Rest)
    end;
send_events(#xml_stream{callback = {process, Pid}} = Stream,
  [Event | Rest]) ->
    case catch Pid ! Event of
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        _ ->
            send_events(Stream, Rest)
    end;
send_events(#xml_stream{callback = {apply, {M, F, Extra}}} = Stream,
  [Event | Rest]) ->
    case catch M:F(Event, Extra) of
        {error, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        _ ->
            send_events(Stream, Rest)
    end;
send_events(#xml_stream{callback = no_callback} = Stream, Events) ->
    {ok, Stream, Events};
send_events(Stream, [Event | Rest]) ->
    error_logger:info_msg("~s:send_event/2: Event: ~p~n", [?MODULE, Event]),
    send_events(Stream, Rest);
send_events(Stream, []) ->
    {ok, Stream}.

% --------------------------------------------------------------------
% Document parsing.
% --------------------------------------------------------------------

%% @spec (Data) -> XML_Element | {error, Reason}
%%     Data = string() | binary()
%%     XML_Element = exmpp_xml:xmlelement()
%% @doc Parse the given data.
%%
%% The XML parser is created with default options.
%%
%% @see exmpp_xml:start_parser/0.
%% @see exmpp_xml:parse_document/1.

parse_element(Data) ->
    parse_element(Data, []).

%% @spec (Data, Parser_Options) -> XML_Element | {error, Reason}
%%     Data = string() | binary()
%%     Parser_Options = [exmpp_xml:xmlparseroption()]
%%     XML_Element = exmpp_xml:xmlelement()
%% @doc Parse the given data.
%%
%% The XML parser is created with given `Parser_Options' options.
%%
%% @see exmpp_xml:start_parser/1.
%% @see exmpp_xml:parse_document/2.

parse_element(Data, Parser_Options) ->
    case exmpp_xml:parse_document(Data, Parser_Options) of
        {ok, done} ->
            {error, parse_error};
        {ok, []} ->
            {error, parse_error};
        {ok, [XML_Element | _]} ->
            XML_Element;
        {xmlerror, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type callback() = Gen_Fsm | Process | Function | No_Callback | Log
%%     Gen_Fsm = {gen_fsm, Pid_or_Name}
%%     Pid_or_Name = pid() | atom()
%%     Process = {process, pid()} | pid()
%%     Function = {apply, {Mod, Func, Extra}}
%%         Mod = atom()
%%         Func = atom()
%%         Extra = term()
%%     No_Callback = no_callback
%%     Log = term().
%% Represents the recipient of each event.
%%
%% In case of a `Gen_Fsm', the call will be:
%% ```
%% gen_fsm:send_event(Pid_or_Name, Event)
%% '''
%% See {@link gen_fsm} documentation for more informations.
%%
%% In case of a `Process', the event is sent using the Erlang `!' operator.
%%
%% In case of a `Function', the call will be:
%% ```
%% Mod:Func(Event, Extra)
%% '''
%% So this function must have an arity of 2.
%%
%% In case of `No_Callback', events are accumulated and returned to
%% {@link parse/2} caller.
%%
%% For `Function', it must return `{error, Reason}' if an error occured
%% or anything else if the event is accepted.
%%
%% If the callback() doesn't match any of these specifications
%% (the `Log' case), the event will be logged with
%% {@link error_logger:info_msg/2}.

%% @type xmlstream().
%% Handler for the opened stream, initialized with a call to {@link
%% start/0} or {@link start/1}.

%% @type xmlstreamevent() = Stream_Start | Stream_Element | Stream_End | Error
%%     Stream_Start = {xmlstreamstart, XML_Element}
%%     Stream_Element = {xmlstreamelement, XML_Element}
%%     Stream_End = {xmlstreamend, XML_End_Element}
%%       XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%%       XML_End_Element = exmpp_xml:xmlnsendelement() | exmpp_xml:xmlendelement()
%%     Stream_Error = {xmlstreamerror, Reason}.
%% Records representing an event sent by the {@link parse/2} function.
