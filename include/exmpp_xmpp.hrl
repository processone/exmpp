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

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

% IQ stanza.
-record(iq, {
  kind    ,%:: request | response,
  type    ,%:: binary(),  %<<"get">> | <<"set">> | <<"result">> | <<"error">>,
  id      ,%:: binary() | undefined,
  ns      ,%:: binary() | undefined,
  payload ,%:: exxml:xmlel() | undefined,
  error   ,%:: exxml:xmlel() | undefined,
  lang    %:: binary() | undefined
}).

% --------------------------------------------------------------------
% Macros for common tests.
% --------------------------------------------------------------------

% Guard expression to test a stanza as defined by XMPP-IM.
-define(IS_IM_STANZA(El), element(1,El) == xmlel).

% Guard expression to test a message.
-define(IS_MESSAGE(El), (
  ?IS_IM_STANZA(El) andalso element(2, El) == <<"message">>
)).

% Guard expression to test a presence.
-define(IS_PRESENCE(El), (
  ?IS_IM_STANZA(El) andalso element(2, El) == <<"presence">>
)).

% Guard expression to test an IQ.
-define(IS_IQ(El), (
  ?IS_IM_STANZA(El) andalso element(2,El) == <<"iq">>
)).
-define(IS_IQ_RECORD(IQ), (
  is_record(IQ, iq)
)).

% Guard expression to test a JID.
-define(IS_JID(Jid), (
  element(1, Jid) =:= 'jid' andalso tuple_size(Jid) =:= 5
)).

% --------------------------------------------------------------------
% Macros to represent <iq/>
% --------------------------------------------------------------------

-define(IQ(Type, From, To, Id, Children),
(
    exxml:element(undefined, <<"iq">>,
        [{<<"type">>, Type},
         {<<"id">>,
          case Id of
            undefined -> exmpp_utils:random_id(<<"iq-">>);
            _         -> Id
          end}
        |
        case {From, To} of
            {undefined, undefined} ->
                [];
            {_,         undefined} ->
                [exxml:attribute(<<"from">>, From)];
            {undefined, _} ->
                [exxml:attribute(<<"to">>, To)];
            {_,         _} ->
                [exxml:attribute(<<"to">>, To),
                 exxml:attribute(<<"from">>, From)]
        end],
        Children)
)).

-define(IQ_GET(From, To, Id, Xmlel),
(
    ?IQ(<<"get">>, From, To, Id, [Xmlel])
)).

-define(IQ_SET(From, To, Id, Xmlel),
(
    ?IQ(<<"set">>, From, To, Id, [Xmlel])
)).
