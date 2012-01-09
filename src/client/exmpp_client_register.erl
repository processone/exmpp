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

-export([
    get_registration_fields/0,
    get_registration_fields/1,
    register_account/1,
    register_account/2,
    remove_account/0,
    remove_account/1
]).

%%
-export_type([
    field1/0,
    fields/0
]).

-type(field1() :: {Name :: binary(), Value :: binary()}).
-type(fields() :: [Field::exmpp_client_register:field1()]).

%%
-define(Xmlel(Name, Attrs, Children),
(
    exxml:element(undefined, Name, Attrs, Children)
)).

%%
-define(Xmlel@InBand_Register(Name, Attrs, Children),
(
    exxml:element(?NS_INBAND_REGISTER, Name, Attrs, Children)
)).

%% @spec () -> Register_Iq
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to get the instruction to register and the list
%% of registration fields.
%%
%% The stanza `id' is generated automatically.

-spec(get_registration_fields/0 :: () -> Stanza_IQ_Get::exmpp_stanza:iq_get()).

get_registration_fields() ->
    get_registration_fields(register_id()).

%% @spec (Id) -> Register_Iq
%%     Id = binary()
%%     Register_Iq = exxml:xmlel()
%% @doc Make an `<iq>' to get the instruction to register and the list
%% of registration fields.

-spec(get_registration_fields/1 ::
(
  Id::exmpp_stanza:id())
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

get_registration_fields(Id) ->
    %% Make empty query
    ?IQ_GET(undefined, undefined, Id,
        ?Xmlel@InBand_Register(<<"query">>, [], [])).

%% @doc Make an `<iq>' that prepare a registration packet for the user.
-spec(register_account/1 ::
(
  Fields::exmpp_client_register:fields())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

register_account(Fields) ->
    register_account(register_id(), Fields).

%% @doc Make an `<iq>' that prepare a registration packet for the user.
-spec(register_account/2 ::
(
  Id     :: exmpp_stanza:id(),
  Fields :: exmpp_client_register:fields())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

register_account(Id, Fields) ->
    ?IQ_SET(undefined, undefined, Id,
        ?Xmlel@InBand_Register(<<"query">>, [],
            [?Xmlel(Name, [], [exxml:cdata(Value)]) || {Name, Value} <- Fields])
        ).

%% @spec () -> RemoveRegister_Iq
%%     RemoveRegister_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that delete user account on the server. The
%% user is supposed to be already logged in.

-spec(remove_account/0 :: () -> Stanza_IQ_Set::exmpp_stanza:iq_set()).

remove_account() ->
    remove_account(register_id()).

%% @spec (Id) -> RemoveRegister_Iq
%%     Id = string()
%%     RemoveRegister_Iq = exxml:xmlel()
%% @doc Make an `<iq>' that delete user account on the server. The
%% user is supposed to be already logged in.

-spec(remove_account/1 ::
(
  Id::exmpp_stanza:id())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

remove_account(Id) ->
    ?IQ_SET(undefined, undefined, Id,
        ?Xmlel@InBand_Register(<<"query">>, [], [
            ?Xmlel(<<"remove">>, [], [])
        ])).

%% TODO: register_form



%% @doc Generate a random register iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

-spec(register_id/0 :: () -> Id::exmpp_stanza:id()).

register_id() ->
    exmpp_utils:random_id(<<"reg-">>).
