% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helper to do IQ common
%% operations.

-module(exmpp_iq).
-vsn('$Revision$').

-include("exmpp.hrl").

% Creation.
-export([
  get/2,
  get/3,
  set/2,
  set/3,
  result/1,
  result/2,
  error/2,
  error_without_original/2
]).

% IQ standard attributes.
-export([
  is_iq/1,
  is_request/1,
  is_response/1,
  is_result/1,
  is_error/1,
  get_type/1,
  get_kind/1,
  get_request/1,
  get_result/1,
  get_payload/1
]).

% --------------------------------------------------------------------
% IQ creation.
% --------------------------------------------------------------------

%% @spec (NS, Request) -> IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `get' request.

get(NS, Request) ->
    get(NS, Request, undefined).

%% @spec (NS, Request, ID) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     ID = string()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `get' request.

get(NS, Request, ID) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "get"),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, ID),
    #xmlel{
      ns = NS,
      name = 'iq',
      attrs = Attrs2,
      children = [Request]
    }.

%% @spec (NS, Request) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `set' request.

set(NS, Request) ->
    set(NS, Request, undefined).

%% @spec (NS, Request, ID) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     ID = string()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `set' request.

set(NS, Request, ID) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "set"),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, ID),
    #xmlel{
      ns = NS,
      name = 'iq',
      attrs = Attrs2,
      children = [Request]
    }.

%% @spec (Request_IQ) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Response_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to answer to the given request.

result(Request_IQ) when ?IS_IQ(Request_IQ) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], "result"),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1,
      exmpp_stanza:get_id(Request_IQ)),
    #xmlel{
      ns = Request_IQ#xmlel.ns,
      name = 'iq',
      attrs = Attrs2
    }.

%% @spec (Request_IQ, Result) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Result = exmpp_xml:xmlel() | [exmpp_xml:xmlel()]
%%     Response_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to answer to the given request with `Result'.

result(Request_IQ, Result) when ?IS_IQ(Request_IQ) andalso is_list(Result) ->
    exmpp_xml:set_children(result(Request_IQ), Result);
result(Request_IQ, Result) when ?IS_IQ(Request_IQ) ->
    exmpp_xml:set_children(result(Request_IQ), [Result]).

%% @spec (Request_IQ, Error) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel() | atom()
%%     Response_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to notify an error.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

error(IQ, Condition) when is_atom(Condition) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    error(IQ, Error);
error(IQ, Error) when ?IS_IQ(IQ) ->
    Attrs1 = exmpp_stanza:set_id_in_attrs([], exmpp_stanza:get_id(IQ)),
    exmpp_stanza:stanza_error(IQ#xmlel{attrs = Attrs1}, Error).

%% @spec (Request_IQ, Error) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel() | atom()
%%     Response_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to notify an error.
%%
%% Child elements from `Request_IQ' are not kept.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

error_without_original(IQ, Condition) when is_atom(Condition) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    error_without_original(IQ, Error);
error_without_original(IQ, Error) when ?IS_IQ(IQ) ->
    Attrs1 = exmpp_stanza:set_id_in_attrs([], exmpp_stanza:get_id(IQ)),
    exmpp_stanza:stanza_error_without_original(IQ#xmlel{attrs = Attrs1},
      Error).

% --------------------------------------------------------------------
% IQ standard attributes.
% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is an IQ.
%%
%% You should probably use the `IS_IQ(El)' guard expression.

is_iq(IQ) when ?IS_IQ(IQ) -> true;
is_iq(_El)                -> false.

%% @spec (IQ) -> Type
%%     IQ = exmpp_xml:xmlel()
%%     Type = get | set | result | error | undefined
%% @doc Return the type of the given `<iq/>'.

get_type(IQ) when ?IS_IQ(IQ) ->
    case exmpp_stanza:get_type(IQ) of
        "get"    -> 'get';
        "set"    -> 'set';
        "result" -> 'result';
        "error"  -> 'error';
        _        -> undefined
    end.

%% @spec (IQ) -> Kind
%%     IQ = exmpp_xml:xmlel()
%%     Kind = request | response | undefined
%% @doc Tell if an IQ is a request or a response.

get_kind(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'get'    -> request;
        'set'    -> request;
        'result' -> response;
        'error'  -> response;
        _        -> undefined
    end.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the IQ is a request.

is_request(IQ) when ?IS_IQ(IQ) ->
    case get_kind(IQ) of
        request -> true;
        _       -> false
    end.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the IQ is a response.

is_response(IQ) when ?IS_IQ(IQ) ->
    case get_kind(IQ) of
        response -> true;
        _        -> false
    end.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the IQ is a result (response of type `result').

is_result(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'result' -> true;
        _        -> false
    end.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the IQ is an error (response of type `error').

is_error(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'error' -> true;
        _       -> false
    end.

%% @spec (IQ) -> Request | undefined
%%     IQ = exmpp_xml:xmlel()
%%     Request = exmpp_xml:xmlel()
%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_result, invalid_iq, IQ}
%% @doc Return the request contained in a `get' or `set' IQ, or returned
%% by an `error' IQ (if present).

get_request(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        undefined ->
            throw({iq, get_result, invalid_iq, IQ});
        Type when Type == 'get' orelse Type == 'set' ->
            % We take the first child element. Note that the RFC says
            % that this child element MUST be the only one! This doesn't
            % take into account text nodes.
            [Request | _] = exmpp_xml:get_child_elements(IQ),
            Request;
        'result' ->
            throw({iq, get_request, unexpected_iq, IQ});
        'error' ->
            NS = IQ#xmlel.ns,
            [Request | _] = IQ#xmlel.children,
            case Request of
                #xmlel{ns = NS, name = 'error'} ->
                    undefined;
                _ ->
                    Request
            end
    end.

%% @spec (IQ) -> Result | undefined
%%     IQ = exmpp_xml:xmlel()
%%     Result = exmpp_xml:xmlel()
%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_result, invalid_iq, IQ}
%% @doc Return the result contained in a `result' IQ.

get_result(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        undefined ->
            throw({iq, get_result, invalid_iq, IQ});
        'result' ->
            case IQ#xmlel.children of
                [] ->
                    undefined;
                [Result | _] ->
                    Result
            end;
        _ ->
            throw({iq, get_result, unexpected_iq, IQ})
    end.

%% @spec (IQ) -> Payload
%%     IQ = exmpp_xml:xmlel()
%%     Payload = exmpp_xml:xmlel()
%% @doc Extract the request, the result or the error from `IQ'.

get_payload(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'get'    -> exmpp_iq:get_request(IQ);
        'set'    -> exmpp_iq:get_request(IQ);
        'result' -> exmpp_iq:get_result(IQ);
        'error'  -> exmpp_stanza:get_error(IQ);
        _        -> throw({iq, get_payload, unexpected_iq, IQ})
    end.
