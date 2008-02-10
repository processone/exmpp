% $Id$

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> provides helper to manipulate
%% error returns in XMPP.
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

-module(exmpp_error).

-export([get_reason/1]).
-export([throw/4,
	 posix_message/2]).

-include("exmpp.hrl").

%% Takes an IQElement as input and return the error reason (atom|string)
get_reason(IQElement) when record(IQElement, xmlnselement) ->
    ErrorElement = exmpp_xml:get_element_by_name(IQElement, 'error'),
    ReasonElement = exmpp_xml:
	get_element_by_ns(ErrorElement,
			  'urn:ietf:params:xml:ns:xmpp-stanzas'),
    ReasonElement#xmlnselement.name.

%% Functions to beautify generic Erlang errors

%% throw error with the following format:
%% {ErrorType, Message}
throw(ErrorType, ErrorCode, Message, Args) ->
    Text = lists:flatten(io_lib:format(Message, Args)),
    throw({ErrorType, ErrorCode, Text}).

%% Generate human readable error from posix code		 
posix_message(port_error, enfile) ->
    "enfile: port table is full. Make sure the maximum number of ports is correctly setup (ERL_MAX_PORTS environment variable)";
posix_message(ErrorType, PosixCode) ->
    io_lib:format("~p", [{ErrorType, PosixCode}]).
