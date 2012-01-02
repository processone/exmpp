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
%% The module <strong>{@module}</strong> sends events to a specified
%% process or function based on elements and trees returned by the
%% parser `exxml'.
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

-include("exmpp.hrl").

-export([
	 start/2,
	 reset/1,
	 get_parser/1,
	 stop/1,
	 parse/2,
	 send_events/2,
	 change_callback/2,
	 parse_element/1,
	 parse_element/2,
	 set_wrapper_tagnames/2 %% Used to unwrap BOSH enclosing body
	]).

-type(process()  :: pid() | atom()).
-type(callback() ::
      process()                      |
      {gen_fsm, process()}           |
      {process, process()}           |
      {apply, atom(), atom(), any()} |
      no_callback                    |
      any()
     ).

-record(xml_stream, {
	  callback             :: callback(),
	  parser,
	  opened = false       :: boolean(),
	  wrapper_tagnames = undefined :: undefined | [binary() ]
	 }).
-type(xmlstream() :: #xml_stream{}).

-type(xmlstreamevent() ::
	{xmlstreamstart, binary(), [exxml:xmlattr()]} |
      #xmlstreamstart{} |
      #xmlstreamelement{} |
      #xmlstreamend{}
     ).

%% --------------------------------------------------------------------
%% Stream parsing, chunk by chunk.
%% --------------------------------------------------------------------

%% @spec (Callback, Parser) -> Stream
%%     Callback = callback()
%%     Stream = xmlstream()
%%     Parser = exxml:xmlparser()
%% @doc Start a new stream handler.
%%
%% The XML parser is reset and option `{root_depth, 1}' is set 
%%
%%
%% @see exxml:start_parser/1.
%% @see exxml:reset_parser/2.

-spec(start/2 ::
      (callback(), exxml:xmlparser()) -> xmlstream()).


start(Callback, Parser) ->
    Callback2 = case Callback of
		    Pid when is_pid(Pid) -> {process, Pid};
		    _                    -> Callback
		end,
    ok = exxml:reset_parser(Parser,
					[{root_depth, 1} ]),
    #xml_stream{
		     callback = Callback2,
		     parser = Parser
		    }.

%% @spec (Stream) -> New_Stream
%%     Stream = xmlstream()
%%     New_Stream = xmlstream()
%% @doc Reset stream and the underlying XML parser.

-spec(reset/1 :: (xmlstream()) -> xmlstream()).

reset(#xml_stream{parser = Parser} = Stream) ->
    ok = exxml:reset_parser(Parser),
    Stream#xml_stream{opened = false}.

%% @spec (Stream) -> Parser
%%     Stream = xmlstream()
%%     Parser = exxml:xmlparser()
%% @doc Return the XML parser used.

-spec(get_parser/1 :: (xmlstream()) -> exxml:xmlparser()).

get_parser(#xml_stream{parser = Parser}) ->
    Parser.

%% @spec (Stream) -> ok | {error, Reason}
%%     Stream = xmlstream()
%% @doc Close a stream handler.
%%
%% This must be called when `Stream' (returned by {@link start/2} or
%% {@link start/3} isn't necessary anymore.
%%
%% Currently this is a NOOP.
%%
%% The caller is responsible to terminate the parser.
%%
%% @see get_parser/1.

-spec(stop/1 :: (xmlstream()) -> ok).

stop(_Stream) ->
    ok.

%% @spec (Stream, Data) -> {ok, New_Stream} | {ok, New_Stream, Events} | {error, Reason}
%%     Stream = xmlstream()
%%     Data = binary()
%%     New_Stream = xmlstream()
%%     Events = [xmlstreamevent()]
%% @doc Parse a chunk of XML data and send events to the callback
%% process or function, or return them to the caller.
%%
%% If `Data' doesn't contain one or more complete XML elements, this
%% function may not send any event.
%%
%% Potential events are described by the {@link xmlstreamevent()} type.

-spec(parse/2 ::
      (xmlstream(), binary() ) ->
	     {ok, xmlstream()} | {ok, xmlstream(), [xmlstreamevent()]} |
		 {error, any()}).

parse(#xml_stream{parser = Parser} = Stream, Data) ->
    try exxml:parse(Parser, Data) of
	    {ok, XML_Elements} ->
		    {ok, New_Stream, Events} = process_elements(Stream, XML_Elements),
		    send_events(New_Stream, Events)
    catch
        throw:{xml_parser, parsing, Error, Reason} ->
            send_events(Stream, [{xmlstreamerror, {Error, Reason}}]);
	  throw:Exception ->
            throw(Exception)
    end.

%% No wrapper tag defined:
process_elements(#xml_stream{wrapper_tagnames=undefined} = Stream, XML_Elements) ->
    process_elements2(Stream, XML_Elements, []);
%% Wrapper tags defined:
%% Remove level 1 wrapper tags.
%% Known use case: Remove enclosing body for BOSH support
process_elements(#xml_stream{wrapper_tagnames=TagNames} = Stream, XML_Elements)
  when TagNames /= undefined ->
    New_XML_Elements =
	lists:map(
	  fun({xmlel, Name, _Attrs, Children} = XML_Element) ->
		  case lists:member(Name, TagNames) of
		      true -> case Children of
				      undefined -> [];
				      _ -> Children
				end;
		      false -> XML_Element
		  end;
	      ({xmlelend, Name} = XML_Element)->
		  case lists:member(Name, TagNames) of
		      true -> [];
		      false -> XML_Element
		  end
	  end, XML_Elements),
    %%horrible hack, fixme
    Filtered = lists:flatten(New_XML_Elements),
    Opened = Stream#xml_stream.opened orelse length(Filtered) /= length(XML_Elements),
    process_elements2(Stream#xml_stream{opened = Opened}, Filtered, []);
%% All other cases (for example endtag)
process_elements(Stream, XML_Elements) ->
    process_elements2(Stream, XML_Elements, []).

process_elements2(Stream, [XML_Element | Rest], Events) ->
	io:format("~p|~p \n", [XML_Element, Rest]),
    case XML_Element of
	{xmlel, _Name, _Attrs, _} when Stream#xml_stream.opened == false ->
	    %% Stream is freshly opened.
            New_Stream = Stream#xml_stream{opened = true},
            New_Events =  [#xmlstreamstart{element = XML_Element} | Events],
            process_elements2(New_Stream, Rest, New_Events);
    	{xmlel, _, _,_} ->
	    %% An "depth 1" element and its children.
            New_Events = [#xmlstreamelement{element = XML_Element} |
			  Events],
            process_elements2(Stream, Rest, New_Events);


	%% Common.
	{xmlelend, _Name} ->
	    %% Stream is closed.
            New_Stream = Stream#xml_stream{opened = false},
            New_Events = [#xmlstreamend{endtag = XML_Element} |
			  Events],
            process_elements2(New_Stream, Rest, New_Events);

	%% Character data as <stream> child, ignore.
	%% This is probably a keep-alive whitespace.
	{cdata, _} ->
            process_elements2(Stream, Rest, Events)
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

%% @spec (Stream, CallBack) -> NewStream
%%     Stream = xmlstream()
%%     CallBack = callback()
%%     NewStream = xmlstream()
%% @doc Change callback of the stream.

-spec(change_callback/2 :: (xmlstream(), callback()) -> xmlstream()).

change_callback(Stream, CallBack) ->
    NewCallBack = if is_pid(CallBack) ->
			  {process, CallBack};
		     true ->
			  CallBack
		  end,
    Stream#xml_stream{callback = NewCallBack}.

%% --------------------------------------------------------------------
%% Document parsing.
%% --------------------------------------------------------------------

%% @spec (Data) -> [XML_Element]
%%     Data =  binary()
%%     XML_Element = exxml:xmlel() 
%% @doc Parse the given data.
%%
%% The XML parser is created with default options.
%%
%% @see exxml:start_parser/0.
%% @see exxml:parse_document/1.

-spec(parse_element/1 ::
      (binary() ) ->
	     [exxml:xmlnode() | emxl:xmlendtag()]).

parse_element(Data) ->
    parse_element(Data, []).

%% @spec (Data, Parser_Options) -> [XML_Element]
%%     Data = binary()
%%     Parser_Options = [exxml:xmlparseroption()]
%%     XML_Element = exxml:xmlel() | exxml:xmlcdata()
%% @doc Parse the given data.
%%
%% The XML parser is created with given `Parser_Options' options.
%%
%% @see exxml:start_parser/1.
%% @see exxml:parse_document/2.

-spec(parse_element/2 ::
      (binary() , [exxml:xmlparseroption()]) ->
	     [exxml:xmlnode() | exxml:xmlendtag()]).

parse_element(Data, Parser_Options) ->
    case exxml:parse_document(Data, Parser_Options) of
       {ok, []} ->
            throw({xmlstream, parse_element, parse_error, []});
       {ok, [XML_Element]} ->
            XML_Element
    end.

%% @spec (Stream, TagNames) -> New_Stream
%%     Stream = xmlstream()
%%     TagNames =  [binary()]
%%     New_Stream = xmlstream()
%% @doc Reset stream and the underlying XML parser.
%% TODO: Support wrapper tag match on both namespace and name ?

-spec(set_wrapper_tagnames/2 :: (xmlstream(), [binary()]) -> xmlstream()).

set_wrapper_tagnames(Stream, TagNames) when is_list(TagNames) ->
    Stream#xml_stream{wrapper_tagnames = TagNames}.

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

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
%%     Stream_Start = {xmlstreamstart, XML_Element} | {xmlstreamstart, Name, Attrs}
%%     Stream_Element = {xmlstreamelement, XML_Element}
%%     Stream_End = {xmlstreamend, XML_End_Tag}
%%       XML_Element = exxml:xmlel() 
%%       XML_End_Tag = exxml:xmlendtag()
%%       Name = binary()
%%       Attrs = [exxml:xmlattr()]
%%     Stream_Error = {xmlstreamerror, Reason}.
%% Records representing an event sent by the {@link parse/2} function.
