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
%% The module <strong>{@module}</strong> provides helpers to do IQ common
%% operations.

-module(exmpp_iq).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Creation.
-export([
	 get/1,
	 get/2,
	 set/1,
	 set/2,
	 result/1,
	 result/2,
	 error/2,
	 error/3,
	 error_without_original/2
	]).

%% #iq record conversion.
-export([
	 xmlel_to_iq/1,
	 iq_to_xmlel/1,
	 iq_to_xmlel/3
	]).

%% IQ standard attributes.
-export([
	 is_iq/1,
	 is_iq_record/1,
	 is_request/1,
	 is_response/1,
	 is_result/1,
	 is_error/1,
	 get_type/1,
	 get_kind/1,
	 get_request/1,
	 get_result/1,
	 get_payload/1,
	 get_payload_ns/1
	]).

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type iq() = {iq, Kind, Type, ID, NS, Payload, Error, Lang, IQ_NS}
%%     Kind = request | response
%%     Type = <<"get">> | <<"set">> | <<"result">> | <<"error">>
%%     ID = binary() | undefined
%%     NS = binary() | undefined
%%     Payload = exxml:el() | undefined
%%     Error = exxml:el() | undefined
%%     Lang = binary() | undefined
%%     IQ_NS = binary() | undefined.
%% Record representing an IQ stanza.
%%
%% It's created from an exxml:el() using {@link xmlel_to_iq/1}. This record
%% eases matching in function clauses. It may be passed to functions in
%% {@link exmpp_stanza} and {@link exmpp_iq}. For other operations, it
%% must be converted back to exxml:el() using {@link iq_to_xmlel/1}.


-export_type([
  iq/0,
  %%
  iq_get/0,
  iq_set/0,
  iq_result/0,
  iq_error/0,
  %%
  iq_request/0,
  iq_response/0
]).

-type(iq_get()
  :: #iq{
         kind    :: 'request',
         type    :: exmpp_stanza:type_get(),
         id      :: exmpp_stanza:id() | undefined,
         ns      :: binary() | undefined,
         payload :: exxml:el() | undefined,
         error   :: undefined,
         lang    :: exmpp_stanza:lang() | undefined
     }
).

-type(iq_set()
  :: #iq{
         kind    :: 'request',
         type    :: exmpp_stanza:type_set(),
         id      :: exmpp_stanza:id() | undefined,
         ns      :: binary() | undefined,
         payload :: exxml:el() | undefined,
         error   :: undefined,
         lang    :: exmpp_stanza:lang() | undefined
     }
).

-type(iq_result()
  :: #iq{
         kind    :: 'response',
         type    :: exmpp_stanza:type_result(),
         id      :: exmpp_stanza:id() | undefined,
         ns      :: binary() | undefined,
         payload :: exxml:el() | undefined,
         error   :: undefined,
         lang    :: exmpp_stanza:lang() | undefined
     }
).

-type(iq_error()
  :: #iq{
         kind    :: 'response',
         type    :: exmpp_stanza:type_error(),
         id      :: exmpp_stanza:id() | undefined,
         ns      :: binary() | undefined,
         payload :: exxml:el() | undefined,
         error   :: exmpp_stanza:xmlel_error(),
         lang    :: exmpp_stanza:lang() | undefined
     }
).

-type(iq_request() :: exmpp_iq:iq_get()
                    | exmpp_iq:iq_set()
).

-type(iq_response() :: exmpp_iq:iq_result()
                     | exmpp_iq:iq_error()
).

-type(iq() :: exmpp_iq:iq_request()
            | exmpp_iq:iq_result()
).


-define(Is_Xmlel_Error(Xmlel_Error),
(
  Xmlel_Error#xmlel.name == <<"error">>
)).

%% --------------------------------------------------------------------
%% IQ creation.
%% --------------------------------------------------------------------

%% @doc Prepare an `<iq/>' to transport the given `get' request.
-spec(get/1 ::
(
  Request::exxml:el())
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

get(Request) ->
    get(Request, 'random').

%% @doc Prepare an `<iq/>' to transport the given `get' request.
-spec(get/2 ::
(
  Request :: exxml:el(),
  Id      :: exmpp_stanza:id() | 'random')
    -> Stanza_IQ_Get::exmpp_stanza:iq_get()
).

get(Request, 'random') when is_record(Request, 'xmlel') ->
    get(Request, exmpp_utils:random_id(Request#xmlel.name));
get(Request, Id) when is_record(Request, 'xmlel') andalso is_binary(Id) ->
    exxml:element(undefined, <<"iq">>,
        [{<<"type">>, <<"get">>}, {<<"id">>, Id}], [Request]).

%% @doc Prepare an `<iq/>' to transport the given `set' request.
-spec(set/1 ::
(
  Request::exxml:el())
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

set(Request) ->
    set(Request, 'random').

%% @doc Prepare an `<iq/>' to transport the given `set' request.
-spec(set/2 ::
(
  Request :: exxml:el(),
  Id      :: exmpp_stanza:id() | 'random')
    -> Stanza_IQ_Set::exmpp_stanza:iq_set()
).

set(Request, 'random') ->
    set(Request, exmpp_utils:random_id(Request#xmlel.name));
set(Request, Id) when is_record(Request, 'xmlel') andalso is_binary(Id) ->
    exxml:element(undefined, <<"iq">>, [{<<"type">>, <<"set">>}, {<<"id">>, Id}],
        [Request]).

%% @doc Prepare an `<iq/>' to answer to the given request.
-spec(result/1 ::
(
  Stanza_IQ::exmpp_stanza:iq())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result();
(
  IQ::exmpp_iq:iq())
    -> IQ_Result::exmpp_iq:iq_result()
).

result(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    exxml:set_attribute(
        exmpp_stanza:reply_without_content(Stanza_IQ), <<"type">>, <<"result">>);
result(IQ) when ?IS_IQ_RECORD(IQ) ->
    IQ#iq{
        kind    = 'response',
        type    = <<"result">>,
        ns      = undefined,
        payload = undefined
    }.

%% @doc Prepare an `<iq/>' to answer to the given request with `Result'.
-spec(result/2 ::
(
  Stanza_IQ :: exmpp_stanza:iq(),
  Result    :: exxml:el())
    -> Stanza_IQ_Result::exmpp_stanza:iq_result();
(
  IQ     :: exmpp_iq:iq(),
  Result :: exxml:el())
    -> IQ_Result::exmpp_iq:iq_result()
).

result(Stanza_IQ, Result) when ?IS_IQ(Stanza_IQ) ->
    exxml:append_child(result(Stanza_IQ), Result);
result(IQ, Result) when ?IS_IQ_RECORD(IQ) ->
    IQ#iq{
        kind    = 'response',
        type    = <<"result">>,
        ns      = exxml:get_attribute(Result, <<"xmlns">>, undefined),
        payload = Result
    }.

%% @doc Prepare an `<iq/>' to notify an error.
%%
%% If `Error' is an binary, it must be a standard condition defined by
%% XMPP Core.
-spec(error/2 ::
(
  Stanza_IQ :: exmpp_stanza:iq(),
  _         :: exmpp_stanza:xmlel_error() | exmpp_stanza:error_condition())
    -> Stanza_IQ_Error::exmpp_stanza:iq_error();
(
  IQ :: exmpp_iq:iq(),
  _  :: exmpp_stanza:xmlel_error() | exmpp_stanza:error_condition())
    -> IQ_Error::exmpp_iq:iq_error()
).

error(Stanza_IQ, Error_Condition)
  when is_binary(Error_Condition) andalso ?IS_IQ(Stanza_IQ) ->
    error(Stanza_IQ, exmpp_stanza:error(Error_Condition));
error(IQ, Error_Condition)
  when is_binary(Error_Condition) andalso ?IS_IQ_RECORD(IQ) ->
    error(IQ, exmpp_stanza:error(Error_Condition));
error(Stanza_IQ, Xmlel_Error)
  when ?IS_IQ(Stanza_IQ) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    exmpp_stanza:reply_with_error(Stanza_IQ, Xmlel_Error);
error(IQ, Xmlel_Error)
  when ?IS_IQ_RECORD(IQ) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    IQ#iq{
        kind  = 'response',
        type  = <<"error">>,
        error = Xmlel_Error
    }.

%% @doc Prepare an `<iq/>' to notify an error
%%      with an error <text/>
%% If `Error' is a binary, it must be a standard condition defined by
%% XMPP Core.
-spec(error/3 ::
(
  Stanza_IQ       :: exmpp_stanza:iq(),
  Error_Condition :: exmpp_stanza:error_condition(),
  Error_Text      :: exmpp_stanza:error_text() | undefined)
    -> Stanza_IQ_Error::exmpp_stanza:iq_error();
(
  IQ              :: exmpp_iq:iq(),
  Error_Condition :: exmpp_stanza:error_condition(),
  Error_Text      :: exmpp_stanza:error_text() | undefined)
    -> IQ_Error::exmpp_iq:iq_error()
).

error(Stanza_IQ, Error_Condition, Error_Text)
  when is_binary(Error_Condition) andalso ?IS_IQ(Stanza_IQ) ->
    error(Stanza_IQ, exmpp_stanza:error(Error_Condition, {undefined, Error_Text}));
error(IQ, Error_Condition, Error_Text)
  when is_binary(Error_Condition) andalso ?IS_IQ_RECORD(IQ) ->
    error(IQ, exmpp_stanza:error(Error_Condition, {undefined, Error_Text})).

%% @doc Prepare an `<iq/>' to notify an error.
%%
%% Child elements from `Request_IQ' are not kept.
%%
%% If `Error' is a binary, it must be a standard condition defined by
%% XMPP Core.
-spec(error_without_original/2 ::
(
  Stanza_IQ :: exmpp_stanza:iq(),
  _         :: exmpp_stanza:xmlel_error() | exmpp_stanza:error_condition())
    -> Stanza_IQ_Error::exmpp_stanza:iq_error();
(
  IQ :: exmpp_iq:iq(),
  _  :: exmpp_stanza:xmlel_error() | exmpp_stanza:error_condition())
    -> IQ_Error::exmpp_iq:iq_error()
).

error_without_original(IQ, Error_Condition) when is_binary(Error_Condition) ->
    error_without_original(IQ, exmpp_stanza:error(Error_Condition));
error_without_original(Stanza_IQ, Xmlel_Error)
  when ?IS_IQ(Stanza_IQ) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    exmpp_stanza:reply_with_error(Stanza_IQ#xmlel{children = []}, Xmlel_Error);
error_without_original(IQ, Xmlel_Error)
  when ?IS_IQ_RECORD(IQ) andalso ?Is_Xmlel_Error(Xmlel_Error) ->
    IQ#iq{
        kind    = 'response',
        type    = <<"error">>,
        error   = Xmlel_Error,
        payload = undefined
    }.

%% --------------------------------------------------------------------
%% #iq record conversion.
%% --------------------------------------------------------------------

%% @doc Convert an IQ stanza from its exxml:el() form to its #iq form.
-spec(xmlel_to_iq/1 ::
(
  Stanza_IQ::exmpp_stanza:stanza_iq())
    -> IQ::exmpp_iq:iq()
).

xmlel_to_iq(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    Type = get_type(Stanza_IQ),
    {NS, Payload, Xmlel_Error} = case get_payload(Stanza_IQ) of
        undefined ->
            {undefined, undefined, undefined};
        _Xmlel_Error when Type == <<"error">> ->
            case get_request(Stanza_IQ) of
                undefined ->
                    {undefined, undefined, _Xmlel_Error};
                Request ->
                    {exxml:get_attribute(_Xmlel_Error, <<"xmlns">>),
                     Request,
                     _Xmlel_Error}
            end;
        _Payload ->
            {exxml:get_attribute(_Payload, <<"xmlns">>), _Payload, undefined}
    end,
    #iq{
        kind    = get_kind(Stanza_IQ),
        type    = Type,
        id      = exmpp_stanza:get_id(Stanza_IQ),
        ns      = NS,
        payload = Payload,
        error   = Xmlel_Error,
        lang    = exmpp_stanza:get_lang(Stanza_IQ)
    }.


%% @doc Convert an IQ stanza from its #iq form to its exxml:el() form.
-spec(iq_to_xmlel/1 ::
(
  IQ::exmpp_iq:iq())
    -> Stanza_IQ::exmpp_stanza:iq()
).

iq_to_xmlel(IQ) when ?IS_IQ_RECORD(IQ) ->
    iq_to_xmlel2(IQ, []).

%% @doc Convert an IQ stanza from its #iq form to its exxml:el() form and
%% set the sender and recipient at the same time.
-spec(iq_to_xmlel/3 ::
(
  IQ        :: exmpp_iq:iq(),
  Sender    :: exmpp_stanza:from(),
  Recipient :: exmpp_stanza:to())
    -> Stanza_IQ::exmpp_stanza:iq()
).

iq_to_xmlel(IQ, Sender, Recipient) when ?IS_IQ_RECORD(IQ) ->
    iq_to_xmlel2(IQ,
        [exxml:attribute(<<"from">>, Sender), exxml:attribute(<<"to">>, Recipient)]
    ).

%%
-spec(iq_to_xmlel2/2 ::
(
  IQ        :: exmpp_iq:iq(),
  Attrs     :: [{From :: <<_:32>>, exmpp_stanza:from()}  |
                {To   :: <<_:16>>, exmpp_stanza:to()},...]
             | [])
    -> Stanza_IQ::exmpp_stanza:iq()
).

iq_to_xmlel2(
  #iq{type = Type, id = Id, lang = Lang, payload = Payload, error = Xmlel_Error},
  Attrs) ->
    exxml:element(undefined, <<"iq">>,
        [exxml:attribute(<<"type">>, Type), exxml:attribute(<<"id">>, Id) | Attrs]
        ++
        [exxml:attribute(<<"lang">>, Lang) || Lang /= undefined, Lang /= <<>>],
        case Payload of
            undefined ->
                case (Type == <<"error">> andalso Xmlel_Error /= undefined) of
                    true -> [Xmlel_Error];
                    _    -> []
                end;
            _ ->
                case (Type == <<"error">> andalso Xmlel_Error /= undefined) of
                    true -> [Payload, Xmlel_Error];
                    _    -> [Payload]
                end
        end
    ).


%% --------------------------------------------------------------------
%% IQ standard attributes.
%% --------------------------------------------------------------------

%% @doc Tell if `El' is an IQ.
%%
%% You should probably use the `IS_IQ(El)' guard expression.
-spec(is_iq/1 ::
(
  Stanza_IQ :: exmpp_stanza:stanza_iq() | any())
    -> Is_IQ::boolean()
).

is_iq(Stanza_IQ) when ?IS_IQ(Stanza_IQ) -> true;
is_iq(_)                                -> false.

%% @doc Tell if `El' is an IQ record.
%%
%% You should probably use the `IS_IQ_RECORD(El)' guard expression.
-spec(is_iq_record/1 ::
(
  IQ :: exmpp_iq:iq() | any()) -> boolean()).

is_iq_record(IQ) when ?IS_IQ_RECORD(IQ) -> true;
is_iq_record(_)                         -> false.

%% @doc Return the type of the given `<iq/>'.
-spec(get_type/1 ::
(
  Stanza_IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Type :: exmpp_stanza:iq_type() | undefined
).

get_type(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    exmpp_stanza:get_type(Stanza_IQ); 
get_type(#iq{type = Type}) ->
    Type.

%% @doc Tell if an IQ is a request or a response.
-spec(get_kind/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Kind :: 'request' | 'response' | undefined
).

get_kind(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_type(Stanza_IQ) of
        <<"get">>    -> 'request';
        <<"set">>    -> 'request';
        <<"result">> -> 'response';
        <<"error">>  -> 'response';
        _            -> undefined
    end;
get_kind(#iq{kind = Kind}) ->
    Kind.

%% @doc Tell if the IQ is a request.
-spec(is_request/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Is_Request::boolean()
).

is_request(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_kind(Stanza_IQ) of
        'request' -> true;
        _         -> false
    end;
is_request(#iq{kind = Kind}) ->
    Kind == 'request'.

%% @doc Tell if the IQ is a response.
-spec(is_response/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Is_Response::boolean()
).

is_response(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_kind(Stanza_IQ) of
        'response' -> true;
        _          -> false
    end;
is_response(#iq{kind = Kind}) ->
    Kind == 'response'.

%% @doc Tell if the IQ is a result (response of type `result').
-spec(is_result/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Is_Result::boolean()
).

is_result(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_type(Stanza_IQ) of
        <<"result">> -> true;
        _            -> false
    end;
is_result(#iq{type = Type}) ->
    Type == <<"result">>.

%% @doc Tell if the IQ is an error (response of type `error').
-spec(is_error/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Is_Error::boolean()
).

is_error(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_type(Stanza_IQ) of
        <<"error">> -> true;
        _           -> false
    end;
is_error(#iq{type = Type}) ->
    Type == <<"error">>.

%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_request, invalid_iq, IQ}
%% @doc Return the request contained in a `get' or `set' IQ, or returned
%% by an `error' IQ (if present).
-spec(get_request/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Request :: exxml:el() | undefined
).

get_request(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_type(Stanza_IQ) of
        undefined ->
            throw({iq, get_request, 'invalid_iq', Stanza_IQ});
        Type when Type == <<"get">> orelse Type == <<"set">> ->
        %% We take the first child element. Note that the RFC says
        %% that this child element MUST be the only one! This doesn't
        %% take into account text nodes.
            [Request | _] = exxml:get_elements(Stanza_IQ),
            Request;
        <<"result">> ->
            throw({iq, get_request, 'unexpected_iq', Stanza_IQ});
        <<"error">> ->
            case exxml:get_elements(Stanza_IQ) of
                [Xmlel_Error | Xmlels] when ?Is_Xmlel_Error(Xmlel_Error) ->
                    case Xmlels of
                        []            -> undefined;
                        [Request | _] -> Request
                    end;
                [Request | _] ->
                    Request
            end
    end;
get_request(#iq{kind = 'request', payload = Request}) ->
    Request;
get_request(#iq{type = <<"error">>, payload = Request}) ->
    Request;
get_request(#iq{} = IQ) ->
    throw({iq, get_request, 'unexpected_iq', IQ}).

%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_result, invalid_iq, IQ}
%% @doc Return the result contained in a `result' IQ.
-spec(get_result/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Result :: exxml:el() | undefined
).

get_result(Stanza_IQ) when ?IS_IQ(Stanza_IQ) ->
    case get_type(Stanza_IQ) of
        undefined ->
            throw({iq, get_result, 'invalid_iq', Stanza_IQ});
        <<"result">> ->
            case exxml:get_elements(Stanza_IQ) of
                []           -> undefined;
                [Result | _] -> Result
            end;
        _ ->
            throw({iq, get_result, 'unexpected_iq', Stanza_IQ})
    end;
get_result(#iq{type = <<"result">>, payload = Result}) ->
    Result;
get_result(#iq{} = IQ) ->
    throw({iq, get_result, 'unexpected_iq', IQ}).

%% @throws {iq, get_payload, unexpected_iq, IQ}
%% @doc Extract the request, the result or the error from `IQ'.
-spec(get_payload/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> Payload :: exxml:el() | undefined
).

get_payload(IQ) ->
    case exmpp_iq:get_type(IQ) of
        <<"get">>    -> exmpp_iq:get_request(IQ);
        <<"set">>    -> exmpp_iq:get_request(IQ);
        <<"result">> -> exmpp_iq:get_result(IQ);
        <<"error">>  -> exmpp_stanza:get_error(IQ);
        _            -> throw({iq, get_payload, 'invalid_iq', IQ})
    end.

%%
-spec(get_payload_ns/1 ::
(
  IQ :: exmpp_stanza:stanza_iq() | exmpp_iq:iq())
    -> NS :: binary() | undefined
).

get_payload_ns(Stanza_IQ) ->
    exxml:get_attribute(get_payload(Stanza_IQ), <<"xmlns">>).

