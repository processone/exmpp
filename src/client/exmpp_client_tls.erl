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
%% entity side of the TLS feature.
%%
%% <p>
%% Note that it doesn't implement encryption, only feature negotiation
%% at the XMPP level.
%% </p>

-module(exmpp_client_tls).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 announced_support/1
	]).

%% TLS negotiation.
-export([
	 starttls/0
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> Support
%%     Features_Announcement = exxml:xmlel()
%%     Support = none | optional | required
%% @throws {tls, announced_support, invalid_announcement, El}
%% @doc Return the kind of TLS negotiation the receiving entity asks for.

announced_support({xmlel, F, _, _} = El) when F == <<"features">> orelse F == <<"stream:features">> ->
    case exxml:get_element(El, <<"starttls">>) of
        undefined -> none;
        Child     -> announced_support2(Child)
    end.

announced_support2(El) ->
	case exxml:get_elements(El) of
		[] ->   optional;
		[{xmlel, <<"required">>, _, _}] -> required;
		_ -> throw({tls, announced_support, invalid_announcement, El})
	end.

%% --------------------------------------------------------------------
%% TLS negotiation.
%% --------------------------------------------------------------------

%% @spec () -> STARTTLS
%%     STARTTLS = exxml:xmlel()
%% @doc Make an XML element to tell the receiving entity that we want to
%% use TLS.

starttls() ->
	{xmlel, <<"starttls">>, [{<<"xmlns">>, ?NS_TLS}], []}.
