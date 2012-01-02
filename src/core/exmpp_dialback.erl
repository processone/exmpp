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
%% The module <strong>{@module}</strong> provides functions to handle
%% server dialback method.

-module(exmpp_dialback).

-include("exmpp.hrl").

%% Creating elements.
-export([
	 key/3,
	 verify_request/4,
	 verify_response/2,
	 validate/1,
	 validate/2
	]).

%% --------------------------------------------------------------------
%% Creating elements.
%% --------------------------------------------------------------------

%% @spec (From, To, Key) -> Result
%%     From =  binary()
%%     To =  binary()
%%     Key = binary() 
%%     Result = exxml:xmlel()
%% @doc Prepare a `<db:result/>' element to send the key to the
%% Receiving Server.

key(From, To, Key) ->
	{xmlel, <<?NS_DIALBACK_pfx/binary,":result">>, 
		[{<<"from">>, From}, {<<"to">>, To}],
		[{cdata, Key}]}.

%% @spec (From, To, ID, Key) -> Request
%%     From = binary()
%%     To = binary()
%%     ID = binary() | random
%%     Key = binary() 
%%     Request = exxml:xmlel()
%% @doc Prepare a `<db:verify/>' element to send to the Authoritative
%% Server.

verify_request(From, To, ID, random) ->
	verify_request(From, To, ID, exmpp_utils:random_id(<<"verify">>));
verify_request(From, To, ID, Key) ->
	{xmlel, <<?NS_DIALBACK_pfx/binary, ":verify">>, 
		[{<<"from">>, From}, {<<"to">>, To}, {<<"id">>, ID}], 
		[{cdata, Key}]}.


%% @spec (Request, Is_Valid) -> Response
%%     Request = exxml:xmlel()
%%     Is_Valid = boolean()
%%     Response = exxml:xmlel()
%% @doc Prepare a `<db:verify/>' element to answer to the Receiving
%% Server.

verify_response(Request, Is_Valid) ->
    Response = exmpp_stanza:reply_without_content(Request),
    case Is_Valid of
        true  -> exxml:set_attribute(Response, <<"type">>, <<"valid">>);
        false -> exxml:set_attribute(Response,<<"type">>, <<"invalid">>)
    end.

%% @spec (Result) -> Response
%%     Result = exxml:xmlel()
%%     Response = exxml:xmlel()
%% @doc Prepare a `<db:result/>' element to answer to the Originating
%% Server.

validate(Result) ->
    Response = exmpp_stanza:reply_without_content(Result),
    exxml:set_attribute(Response,<<"type">>, <<"valid">>).

%% @spec (From, To) -> Response
%%     From = binary()
%%     To =  binary()
%%     Response = exxml:xmlel()
%% @doc Prepare a `<db:result/>' element to answer to the Originating
%% Server.

validate(From, To) ->
	{xmlel, <<?NS_DIALBACK_pfx/binary, ":result">>, 
		[{<<"from">>, From}, {<<"to">>, To}, {<<"type">>, <<"valid">>}], []}.
