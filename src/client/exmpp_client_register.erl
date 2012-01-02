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

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%%
%% The module <strong>{@module}</strong> implements packets formatting
%% conforming to XEP-0077: In-Band Registration.
%% See: http://www.xmpp.org/extensions/xep-0077.html
%%
%% Note: This implementation is still partial and does not support all
%% workflow of the XEP-0077.

-module(exmpp_client_register).

-include("exmpp.hrl").

-export([get_registration_fields/0,
	 get_registration_fields/1,
	 register_account/1, register_account/2,
	 remove_account/0, remove_account/1]).

%% @spec () -> Register_Iq
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to get the instruction to register and the list
%% of registration fields.
%%
%% The stanza `id' is generated automatically.

get_registration_fields() ->
    get_registration_fields(register_id()).

%% @spec (Id) -> Register_Iq
%%     Id = binary()
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to get the instruction to register and the list
%% of registration fields.

get_registration_fields(Id) ->
    %% Make empty query
    Query = {xmlel, <<"query">>, [{<<"xmlns">>, ?NS_INBAND_REGISTER}], []},
    {xmlel, <<"iq">>, [{<<"type">>, <<"get">>}, {<<"id">>, Id}], [Query]}.

%% @spec (Fields) -> Register_Iq
%%     Fields = [Field]
%%     Field = {Fieldname, Value}
%%     Fieldname = binary()
%%     Value = binary()
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that prepare a registration packet for the user.
register_account(Fields) ->
    register_account(register_id(), Fields).

%% @spec (Id, Fields) -> Register_Iq
%%     Id = binary()
%%     Fields = [Field]
%%     Field = {Fieldname, Value}
%%     Fieldname = binary()
%%     Value = binary()
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that prepare a registration packet for the user.
register_account(Id, Fields) ->
    %% Make query tag
    Query =  {xmlel, <<"query">>, [{<<"xmlns">>, ?NS_INBAND_REGISTER}], []},
    %% Add fields to the query tag
    PreparedQuery = append_fields(Query, Fields),
    %% Put the prepared query in IQ
    {xmlel, <<"iq">>, [{<<"type">>, <<"set">>}, {<<"id">>, Id}],
	    [PreparedQuery]}.

%% @spec () -> RemoveRegister_Iq
%%     RemoveRegister_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that delete user account on the server. The
%% user is supposed to be already logged in.
remove_account() ->
    remove_account(register_id()).

%% @spec (Id) -> RemoveRegister_Iq
%%     Id = string()
%%     RemoveRegister_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that delete user account on the server. The
%% user is supposed to be already logged in.
remove_account(Id) ->
    %% Make query tag
    Remove = {xmlel, <<"remove">>, [], []},
    Query  = {xmlel, <<"query">>, [{<<"xmlns">>, ?NS_INBAND_REGISTER}], [Remove]},
    %% Put the prepared query in IQ
    {xmlel, <<"iq">>, [{<<"type">>, <<"set">>}, {<<"id">>, Id}], [Query]}.

%% @hidden
%% @doc Append each register request field to the query and return the
%% prepared query
append_fields(PreparedQuery, []) ->
    PreparedQuery;
append_fields(Query, [{Field, Value}|Fields]) ->
	FieldElement = {xmlel, Field, [], [{cdata, Value}]},
    	UpdatedQuery = exxml:append_child(Query, FieldElement),
	append_fields(UpdatedQuery, Fields).

%% TODO: register_form



%% @spec () -> Register_ID
%%     Register_ID = binary()
%% @doc Generate a random register iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

register_id() ->
	exmpp_utils:random_id(<<"reg-">>).
