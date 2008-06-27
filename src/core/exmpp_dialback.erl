% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle
%% server dialback method.

-module(exmpp_dialback).
-vsn('$Revision$').

-include("exmpp.hrl").

% Creating elements.
-export([
  key/3,
  verify_request/4,
  verify_response/2,
  validate/1,
  validate/2
]).

% --------------------------------------------------------------------
% Creating elements.
% --------------------------------------------------------------------

%% @spec (From, To, Key) -> Result
%%     From = exmpp_jid:jid() | string()
%%     To = exmpp_jid:jid() | string()
%%     Key = binary() | string()
%%     Result = exmpp_xml:xmlel()
%% @doc Prepare a `<db:result/>' element to send the key to the
%% Receiving Server.

key(From, To, Key) ->
    Attrs1 = exmpp_stanza:set_sender_in_attrs([], From),
    Attrs2 = exmpp_stanza:set_recipient_in_attrs(Attrs1, To),
    Result = #xmlel{
      ns = ?NS_DIALBACK,
      declared_ns = [{?NS_DIALBACK, ?NS_DIALBACK_pfx}],
      name = 'result',
      attrs = Attrs2
    },
    exmpp_xml:set_cdata(Result, Key).

%% @spec (From, To, ID, Key) -> Request
%%     From = exmpp_jid:jid() | string()
%%     To = exmpp_jid:jid() | string()
%%     ID = string()
%%     Key = binary() | string()
%%     Request = exmpp_xml:xmlel()
%% @doc Prepare a `<db:verify/>' element to send to the Authoritative
%% Server.

verify_request(From, To, ID, Key) ->
    Attrs1 = exmpp_stanza:set_sender_in_attrs([], From),
    Attrs2 = exmpp_stanza:set_recipient_in_attrs(Attrs1, To),
    Attrs3 = exmpp_stanza:set_id_in_attrs(Attrs2, ID),
    Request = #xmlel{
      ns = ?NS_DIALBACK,
      declared_ns = [{?NS_DIALBACK, ?NS_DIALBACK_pfx}],
      name = 'verify',
      attrs = Attrs3
    },
    exmpp_xml:set_cdata(Request, Key).

%% @spec (Request, Is_Valid) -> Response
%%     Request = exmpp_xml:xmlel()
%%     Is_Valid = bool()
%%     Response = exmpp_xml:xmlel()
%% @doc Prepare a `<db:verify/>' element to answer to the Receiving
%% Server.

verify_response(Request, Is_Valid) ->
    Response = exmpp_stanza:reply_without_content(Request),
    case Is_Valid of
        true  -> exmpp_stanza:set_type(Response, "valid");
        false -> exmpp_stanza:set_type(Response, "invalid")
    end.

%% @spec (Result) -> Response
%%     Result = exmpp_xml:xmlel()
%%     Response = exmpp_xml:xmlel()
%% @doc Prepare a `<db:result/>' element to answer to the Originating
%% Server.

validate(Result) ->
    Response = exmpp_stanza:reply_without_content(Result),
    exmpp_stanza:set_type(Response, "valid").

%% @spec (From, To) -> Response
%%     From = exmpp_jid:jid() | string()
%%     To = exmpp_jid:jid() | string()
%%     Response = exmpp_xml:xmlel()
%% @doc Prepare a `<db:result/>' element to answer to the Originating
%% Server.

validate(From, To) ->
    Attrs1 = exmpp_stanza:set_sender_in_attrs([], From),
    Attrs2 = exmpp_stanza:set_recipient_in_attrs(Attrs1, To),
    Attrs3 = exmpp_stanza:set_type_in_attrs(Attrs2, "valid"),
    #xmlel{
      ns = ?NS_DIALBACK,
      declared_ns = [{?NS_DIALBACK, ?NS_DIALBACK_pfx}],
      name = 'result',
      attrs = Attrs3
    }.
