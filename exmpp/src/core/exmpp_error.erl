% $Id: $

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> provides helper to manipulate
%% error returns in XMPP.
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

-module(exmpp_error).

-export([get_reason/1]).

-include("exmpp.hrl").

%% Takes an IQElement as input and return the error reason (atom|string)
get_reason(IQElement) when record(IQElement, xmlnselement) ->
    ErrorElement = exmpp_xml:get_element_by_name(IQElement, 'error'),
    ReasonElement = exmpp_xml:
	get_element_by_ns(ErrorElement,
			  'urn:ietf:params:xml:ns:xmpp-stanzas'),
    ReasonElement#xmlnselement.name.
