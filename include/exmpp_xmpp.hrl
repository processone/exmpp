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
  kind    :: request | response,
  type    :: binary(),  %<<"get">> | <<"set">> | <<"result">> | <<"error">>,
  id      :: binary() | undefined,
  ns      :: binary() | undefined,
  payload :: exml:xmlel() | undefined,
  error   :: exml:xmlel() | undefined,
  lang    :: binary() | undefined
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

-define(IQ(Type, To, Id), (
		{xmlel, <<"iq">>, [{<<"type">>, Type}, {<<"to">>, To}, {<<"id">>, Id}], []}
)).

-define(IQ_GET(To, Id), (
  ?IQ(<<"get">>, To, Id)
)).

-define(IQ_SET(To, Id), (
  ?IQ(<<"set">>, To, Id)
)).
