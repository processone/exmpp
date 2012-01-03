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

-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, <<?NS_DIALBACK_pfx/binary, ":", Name/binary>>, Attrs,
        Children)
)).

-export_type([
  from/0,
  to/0,
  id/0,
  key/0
]).

-type(from() :: binary()).
-type(to()   :: binary()).
-type(key()  :: binary()).
-type(id()   :: binary()).

-export_type([
  type/0,
  type_valid/0,
  type_invalid/0,
  type_error/0
]).

-type(type_valid()   :: <<_:40>>).
-type(type_invalid() :: <<_:56>>).
-type(type_error()   :: <<_:40>>).

-type(type() :: exmpp_dialback:type_valid()
              | exmpp_dialback:type_invalid()
              | exmpp_dialback:type_error()
).

-export_type([
  xmlel_db_verify/0
]).

-type(xmlel_db_verify()
  :: #xmlel{
         name     :: <<_:48>>,
         attrs    :: [
             {From :: <<_:32>>, exmpp_dialback:from()} |
             {To   :: <<_:16>>, exmpp_dialback:to()}   |
             {Id   :: <<_:16>>, exmpp_dialback:id()}   ,...
         ],
         children :: [exxml:cdata()]
     }
).

-type(xmlel_db_result()
  :: #xmlel{
         name     :: <<_:48>>,
         attrs    :: [
             {From :: <<_:32>>, exmpp_dialback:from()} |
             {To   :: <<_:16>>, exmpp_dialback:to()}   |
             {Id   :: <<_:16>>, exmpp_dialback:id()}   |
             {Type :: <<_:32>>, exmpp_dialback:type()} ,...
         ],
         children :: [exxml:cdata()]
     }
).

%% --------------------------------------------------------------------
%% Creating elements.
%% --------------------------------------------------------------------

%% @doc Prepare a `<db:result/>' element to send the key to the
%% Receiving Server.
-spec(key/3 ::
(
  From :: exmpp_dialback:from(),
  To   :: exmpp_dialback:to(),
  Key  :: exmpp_dialback:key())
    -> Xmlel_Db_Result::exmpp_dialback:xmlel_db_result()
).

key(From, To, Key) ->
    ?Xmlel(<<"result">>,
        [exxml:attribute(<<"from">>, From),
         exxml:attribute(<<"to">>, To)],
        [exxml:cdata(Key)]).

%% @doc Prepare a `<db:verify/>' element to send to the Authoritative Server.
-spec(verify_request/4 ::
(
  From :: exmpp_dialback:from(),
  To   :: exmpp_dialback:to(),
  Id   :: exmpp_dialback:id(),
  Key  :: exmpp_dialback:key() | 'random')
    -> Xmlel_Db_Verify::exmpp_dialback:xmlel_db_verify()
).

verify_request(From, To, Id, 'random') ->
    verify_request(From, To, Id, exmpp_utils:random_id(<<"verify">>));
verify_request(From, To, Id, Key) ->
    ?Xmlel(<<"verify">>,
        [exxml:attribute(<<"from">>, From),
         exxml:attribute(<<"to">>, To),
         exxml:attribute(<<"id">>, Id)],
        [exxml:cdata(Key)]).

%% @doc Prepare a `<db:verify/>' element to answer to the Receiving Server.
-spec(verify_response/2 ::
(
  Request  :: exmpp_dialback:xmlel_db_verify(),
  Is_Valid :: boolean())
    -> Xmlel_Db_Verify::exmpp_dialback:xmlel_db_verify()
).

verify_response(Request, true = _Is_Valid) ->
    exxml:set_attribute(exmpp_stanza:reply_without_content(Request),
        <<"type">>, <<"valid">>);
verify_response(Request, false = _Is_Valid) ->
    exxml:set_attribute(exmpp_stanza:reply_without_content(Request),
        <<"type">>, <<"invalid">>).

%% @doc Prepare a `<db:result/>' element to answer to the Originating
%% Server.
-spec(validate/1 ::
(
  Result::exmpp_dialback:xmlel_db_result())
    -> Xmlel_Db_Result::exmpp_dialback:xmlel_db_result()
).

validate(Result) ->
    exxml:set_attribute(exmpp_stanza:reply_without_content(Result),
        <<"type">>, <<"valid">>).

%% @doc Prepare a `<db:result/>' element to answer to the Originating Server.
-spec(validate/2 ::
(
  From :: exmpp_dialback:from(),
  To   :: exmpp_dialback:to())
    -> Xmlel_Db_Result::exmpp_dialback:xmlel_db_result()
).

validate(From, To) ->
    ?Xmlel(<<"result">>,
        [exxml:attribute(<<"from">>, From),
         exxml:attribute(<<"to">>, To),
         exxml:attribute(<<"type">>, <<"valid">>)],
        []).

