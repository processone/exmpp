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

%% @spec (Callback) -> {ok, Stream} | {error, Reason}
%%     Callback = callback()
%%     Stream = xmlstream()
%% @doc Start a new stream handler.
%%
%% The XML parser is created with default options, but the `rootdepth'
%% which is 1. This function (or {@link start/1}) must be called before
%% any use of {@link parse/2}.
%%
%% @see exmpp_xml:start_parser/0.
%% @see exmpp_xml:xmlparseroption().
start(Callback) ->
	start(Callback, []).

%% @spec (Callback, Parser_Options) -> {ok, Stream} | {error, Reason}
%%     Callback = callback()
%%     Stream = xmlstream()
%%     Parser_Options = [exmpp_xml:xmlparseroption()]
%% @doc Start a new stream handler.
%%
%% The XML parser is created with `Parser_Options' options (`rootdepth'
%% is 1 by default). This function (or {@link start/0}) must be called
%% before any use of {@link parse/2}.
%%
%% @see exmpp_xml:start_parser/1.
start(Callback, Parser_Options) ->
	Parser_Options2 = [{rootdepth, 1}, endelement | Parser_Options],
	case exmpp_xml:start_parser(Parser_Options2) of
		{ok, Parser} ->
			Callback2 = case Callback of
				Pid when is_pid(Pid) -> {process, Pid};
				_                    -> Callback
			end,
			{ok, #xml_stream{
			    callback = Callback2,
			    parser = Parser
			}};
		{error, Reason} ->
			{error, Reason}
	end.


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

%% @spec (Stream, Data) -> {ok, New_Stream} | {error, Reason}
%%     Stream = xmlstream()
%%     Data = string() | binary()
%%     New_Stream = xmlstream()
%% @doc Parse a chunk of XML data and send events to the callback
%% process or function.
%%
%% If `Data' doesn't contain one or more complete XML elements, this
%% function may not send any event.
%%
%% Potential events are described by the {@link xmlstreamevent()} type.
parse(#xml_stream{parser = Parser} = Stream, Data) ->
	case exmpp_xml:parse(Parser, Data) of
		{ok, continue} ->
			{ok, Stream};
		{ok, XML_Elements} ->
			process_elements(Stream, XML_Elements);
		{xmlerror, Reason} ->
			send_event(Stream,
			    {xmlstreamerror, Reason});
		{error, Reason} ->
			{error, Reason}
	end.

process_elements(Stream, [XML_Element | Rest]) ->
	case XML_Element of
		% Without namespace support.
		#xmlelement{name = Name, attrs = Attrs}
		    when Stream#xml_stream.opened == 0 ->
			% Stream is freshly opened.
			case send_event(Stream,
			    #xmlstreamstart{name = Name, attrs = Attrs}) of
				{ok, New_Stream} ->
					Opened_Stream = New_Stream#xml_stream{
					    opened = 1},
					process_elements(Opened_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;
		#xmlelement{} ->
			% An "depth 1" element and its children.
			case send_event(Stream,
			    #xmlstreamelement{element = XML_Element}) of
				{ok, New_Stream} ->
					process_elements(New_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;
		#xmlendelement{name = Name} ->
			% Stream is closed.
			case send_event(Stream, #xmlstreamend{name = Name}) of
				{ok, New_Stream} ->
					Closed_Stream = New_Stream#xml_stream{
					    opened = 0},
					process_elements(Closed_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;

		% With namespace support.
		#xmlnselement{name = Name, attrs = Attrs}
		    when Stream#xml_stream.opened == 0 ->
			% Stream is freshly opened.
			case send_event(Stream,
			    #xmlstreamstart{name = Name, attrs = Attrs}) of
				{ok, New_Stream} ->
					Opened_Stream = New_Stream#xml_stream{
					    opened = 1},
					process_elements(Opened_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;
		#xmlnselement{} ->
			% An "depth 1" element and its children.
			case send_event(Stream,
			    #xmlstreamelement{element = XML_Element}) of
				{ok, New_Stream} ->
					process_elements(New_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;
		#xmlnsendelement{name =Name} ->
			% Stream is closed.
			case send_event(Stream, #xmlstreamend{name = Name}) of
				{ok, New_Stream} ->
					Closed_Stream = New_Stream#xml_stream{
					    opened = 0},
					process_elements(Closed_Stream, Rest);
				{error, Reason} ->
					{error, Reason}
			end;

		% Unknown tuple.
		_ ->
			error_logger:info_msg(
			    "~s:process_elements/2: Unknown element: ~p~n",
			    [?MODULE, XML_Element]),
			process_elements(Stream, Rest)
	end;
process_elements(Stream, []) ->
	{ok, Stream}.

send_event(#xml_stream{callback = {gen_fsm, Pid}} = Stream, Event) ->
	case catch gen_fsm:send_event(Pid, Event) of
		{error, Reason} ->
			{error, Reason};
		{'EXIT', Reason} ->
			{error, {'EXIT', Reason}};
		_ ->
			{ok, Stream}
	end;
send_event(#xml_stream{callback = {process, Pid}} = Stream, Event) ->
	Pid ! Event,
	receive
		{error, Reason} ->
			{error, Reason};
		_ ->
			{ok, Stream}
	end;
send_event(#xml_stream{callback = {apply, {M, F, Extra}}} = Stream, Event) ->
	case catch M:F(Event, Extra) of
		{error, Reason} ->
			{error, Reason};
		{'EXIT', Reason} ->
			{error, {'EXIT', Reason}};
		_ ->
			{ok, Stream}
	end;
send_event(Stream, Event) ->
	error_logger:info_msg("~s:send_event/2: Event: ~p~n",
	    [?MODULE, Event]),
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

%% @type callback() = Gen_Fsm | Process | Function | No_Callback
%%     Gen_Fsm = {gen_fsm, Pid_or_Name}
%%     Pid_or_Name = pid() | atom()
%%     Process = {process, pid()} | pid()
%%     Function = {apply, {Module, Function, Extra}}
%%     Module = atom()
%%     Function = atom()
%%     Extra = term()
%%     No_Callback = term().
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
%% Module:Function(Event, Extra)
%% '''
%% So this function must have an arity of 2.
%%
%% Regardless what callback solution is chosen, it must return `{error,
%% Reason}' if an error occured or anything else if the event is
%% accepted.
%%
%% If the callback() doesn't match any of these specifications
%% (the `No_Callback' case), the event will be logged with
%% {@link error_logger:info_msg/2}.

%% @type xmlstream().
%% Handler for the opened stream, initialized with a call to {@link
%% start/0} or {@link start/1}.

%% @type xmlstreamevent() = Stream_Start | Stream_Element | Stream_End
%%     Stream_Start = {xmlstreamstart, Name, Start_Attrs}
%%       Start_Name = string()
%%       Attrs = [exmpp_xml:xmlattribute()]
%%     Stream_Element = {xmlstreamelement, XML_Element}
%%       XML_element = exmpp_xml:xmlelement()
%%     Stream_End = {xmlstreamend, Name}.
%% Records representing an event sent by the {@link parse/2} function.
